{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.Catchup
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.Catchup
    ( catchup ) where

--------------------------------------------------------------------------------
import Data.Int
import Data.Maybe
import Data.ProtocolBuffers

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Communication (Transmit(..))
import Database.EventStore.Internal.Control (publishWith)
import Database.EventStore.Internal.Exec (Exec)
import Database.EventStore.Internal.Operation
import qualified Database.EventStore.Internal.Operation.ReadAllEvents.Message as ReadAll
import qualified Database.EventStore.Internal.Operation.ReadStreamEvents.Message as ReadStream
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Settings
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Subscription.Message
import Database.EventStore.Internal.Subscription.Types
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
defaultBatchSize :: Int32
defaultBatchSize = 500

--------------------------------------------------------------------------------
data State s
  = Init s
  | Catchup UUID UUID s
  | Live UUID s

--------------------------------------------------------------------------------
createReadPkg
  :: Settings
  -> StreamId t
  -> t
  -> Int32 -- Batch size
  -> Bool -- Resolve links
  -> Maybe Credentials
  -> IO Package
createReadPkg setts (StreamName stream) evtNum batch tos cred
  = let
      req =
        ReadStream.newRequest
          stream
          (eventNumberToInt64 evtNum)
          batch
          tos
          (s_requireMaster setts) in
    createPkg readStreamEventsForwardCmd cred req
createReadPkg setts All pos batch tos cred
  = let
      req =
        ReadAll.newRequest
          (positionCommit pos)
          (positionPrepare pos)
          batch
          tos
          (s_requireMaster setts) in
    createPkg readAllEventsForwardCmd cred req

--------------------------------------------------------------------------------
catchup
  :: Settings
  -> Exec
  -> StreamId t
  -> t
  -> Bool        -- Resolve link tos.
  -> Maybe Int32 -- Batch size.
  -> Maybe Credentials
  -> IO (TVar (Maybe UUID), Chan SubAction)
catchup setts exec streamId from tos batchSiz cred
  = do m <- mailboxNew
       subM <- newChan
       var <- newTVarIO Nothing
       _ <- async $ keepLoopingS (Init from) $ \case
         Init pos
           -> do let subReq = subscribeToStream stream tos
                 subPkg <- createPkg subscribeToStreamCmd cred subReq
                 readPkg <- createReadPkg setts streamId pos batch tos cred

                 publishWith exec (Transmit m (KeepAlive subscriptionDroppedCmd) subPkg)
                 publishWith exec (Transmit m OneTime readPkg)
                 let theSubId = packageCorrelation subPkg

                 atomically $ writeTVar var (Just theSubId)

                 pure $ LoopS (Catchup theSubId (packageCorrelation readPkg) pos)

         unchanged@(Catchup theSubId readId pos)
           -> do outcome <- mailboxRead m
                 case outcome of
                   Left e
                     -> case e of
                          ConnectionHasDropped
                            -> pure $ LoopS (Init pos)
                          _ -> BreakS () <$ writeChan subM (Dropped SubAborted)

                   Right respPkg
                     | theSubId == packageCorrelation respPkg
                     && packageCmd respPkg == subscriptionDroppedCmd
                     -> let Right resp = decodePkg respPkg
                            reason = fromMaybe D_Unsubscribed (getField $ dropReason resp)
                            subReason = toSubDropReason reason in
                        BreakS () <$ writeChan subM (Dropped subReason)

                     | theSubId == packageCorrelation respPkg
                     && packageCmd respPkg == subscriptionConfirmationCmd
                     -> let Right resp = decodePkg respPkg
                            lcp = getField $ subscribeLastCommitPos resp
                            len = getField $ subscribeLastEventNumber resp
                            details =
                              SubDetails
                              { subId = theSubId
                              , subCommitPos = lcp
                              , subLastEventNum = len
                              , subSubId = Nothing
                              } in
                        LoopS unchanged <$ writeChan subM (Confirmed details)

                     | theSubId == packageCorrelation respPkg
                     -> pure $ LoopS unchanged

                     | readId == packageCorrelation respPkg
                     -> case streamId of
                          StreamName _
                            -> do let
                                    Right resp = decodePkg respPkg
                                    r = getField $ ReadStream._result resp
                                    es = getField $ ReadStream._events resp
                                    evts = fmap newResolvedEvent es
                                    eos = getField $ ReadStream._endOfStream resp
                                    nxt = getField $ ReadStream._nextNumber resp
                                  case r of
                                    ReadStream.NO_STREAM
                                      -> pure $ LoopS (Live theSubId pos)

                                    ReadStream.SUCCESS
                                      -> do traverse_ (writeChan subM . Submit) evts
                                            if eos
                                              then
                                                pure $ LoopS (Live theSubId (rawEventNumber nxt))
                                              else
                                                do newReadPkg <- createReadPkg setts streamId (rawEventNumber nxt) batch tos cred
                                                   let newReadId = packageCorrelation newReadPkg

                                                   publishWith exec (Transmit m OneTime newReadPkg)
                                                   pure $ LoopS (Catchup theSubId newReadId (rawEventNumber nxt))

                                         -- TODO - Do we have to close the subscription?
                                         -- Pretty sure the subcription has failed already at
                                         -- this point.
                                    _ -> BreakS () <$ writeChan subM (Dropped SubAborted)

                          All
                            -> do let
                                    Right resp = decodePkg respPkg
                                    r = getField $ ReadAll._Result resp
                                    nc_pos = getField $ ReadAll._NextCommitPosition resp
                                    np_pos = getField $ ReadAll._NextPreparePosition resp
                                    es = getField $ ReadAll._Events resp
                                    evts = fmap newResolvedEventFromBuf es
                                    eos = null evts
                                    n_pos = Position nc_pos np_pos

                                  case fromMaybe ReadAll.SUCCESS r of
                                    ReadAll.SUCCESS
                                      -> do traverse_ (writeChan subM . Submit) evts
                                            if eos
                                              then
                                                pure $ LoopS (Live theSubId n_pos)
                                              else
                                                do newReadPkg <- createReadPkg setts streamId n_pos batch tos cred
                                                   let newReadId = packageCorrelation newReadPkg

                                                   publishWith exec (Transmit m OneTime newReadPkg)
                                                   pure $ LoopS (Catchup theSubId newReadId n_pos)

                                         -- TODO - Do we have to close the subscription?
                                         -- Pretty sure the subcription has failed already at
                                         -- this point.
                                    _ -> BreakS () <$ writeChan subM (Dropped SubAborted)
                     | otherwise
                     -> pure $ LoopS unchanged

         unchanged@(Live theSubId pos)
           -> do outcome <- mailboxRead m
                 case outcome of
                   Left e
                     -> case e of
                          ConnectionHasDropped
                            -> pure $ LoopS (Init pos)
                          _ -> BreakS () <$ writeChan subM (Dropped SubAborted)

                   Right respPkg
                     | theSubId == packageCorrelation respPkg
                     && packageCmd respPkg == subscriptionDroppedCmd
                     -> let Right resp = decodePkg respPkg
                            reason = fromMaybe D_Unsubscribed (getField $ dropReason resp)
                            subReason = toSubDropReason reason in
                        BreakS () <$ writeChan subM (Dropped subReason)
                     | theSubId == packageCorrelation respPkg
                     && packageCmd respPkg == subscriptionConfirmationCmd
                     -> let Right resp = decodePkg respPkg
                            lcp = getField $ subscribeLastCommitPos resp
                            len = getField $ subscribeLastEventNumber resp
                            details =
                              SubDetails
                              { subId = theSubId
                              , subCommitPos = lcp
                              , subLastEventNum = len
                              , subSubId = Nothing
                              } in
                        LoopS unchanged <$ writeChan subM (Confirmed details)
                     | theSubId == packageCorrelation respPkg
                     && packageCmd respPkg == streamEventAppearedCmd
                     -> let
                          Right resp = decodePkg respPkg
                          evt = newResolvedEventFromBuf $ getField $ streamResolvedEvent resp
                          nextState =
                            case streamId of
                              StreamName _
                                -> let nxt = resolvedEventOriginalEventNumber evt
                                   in Live theSubId (rawEventNumber nxt)
                              All
                                -> let Just nxtPos = resolvedEventPosition evt
                                   in Live theSubId nxtPos in
                        LoopS nextState <$ writeChan subM (Submit evt)
                     | otherwise
                     -> pure $ LoopS unchanged

       pure (var, subM)
  where
    batch = fromMaybe defaultBatchSize batchSiz
    stream = streamIdRaw streamId
