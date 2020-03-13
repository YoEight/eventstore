--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.Persist
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.Persist (persist) where

--------------------------------------------------------------------------------
import Data.ProtocolBuffers
import Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Communication (Transmit(..))
import Database.EventStore.Internal.Control (publishWith)
import Database.EventStore.Internal.Exec (Exec)
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Settings
import Database.EventStore.Internal.Subscription.Message
import Database.EventStore.Internal.Subscription.Types
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
persist
  :: Exec
  -> Text
  -> Text
  -> Int32
  -> Maybe Credentials
  -> IO (UUID, TVar (Maybe Text), Chan SubAction)
persist exec grp stream bufSize cred
  = do m <- mailboxNew
       subM <- newChan
       var <- newTVarIO Nothing
       let req = _connectToPersistentSubscription grp stream bufSize
       pkg <- createPkg connectToPersistentSubscriptionCmd cred req
       let theSubId = packageCorrelation pkg
       publishWith exec (Transmit m (KeepAlive subscriptionDroppedCmd) pkg)
       _ <- async $ keepLooping $
         do outcome <- mailboxRead m
            case outcome of
              Left _
                -> Break () <$ writeChan subM (Dropped SubAborted)
              Right respPkg
                | packageCmd respPkg == subscriptionDroppedCmd
                -> let Right resp = decodePkg respPkg
                       reason = fromMaybe D_Unsubscribed (getField $ dropReason resp)
                       subReason = toSubDropReason reason in
                   Break () <$ writeChan subM (Dropped subReason)
                | packageCmd respPkg == persistentSubscriptionConfirmationCmd
                -> do let Right resp = decodePkg respPkg
                          lcp = getField $ pscLastCommitPos resp
                          subSubId = getField $ pscId resp
                          len = getField $ pscLastEvtNumber resp
                          details =
                            SubDetails
                            { subId = theSubId
                            , subCommitPos = lcp
                            , subLastEventNum = len
                            , subSubId = Just subSubId
                            }
                      atomically $ writeTVar var (Just subSubId)
                      Loop <$ writeChan subM (Confirmed details)
                | packageCmd respPkg == persistentSubscriptionStreamEventAppearedCmd
                -> let Right resp = decodePkg respPkg
                       evt = newResolvedEvent $ getField $ psseaEvt resp in
                   Loop <$ writeChan subM (Submit evt)
                | otherwise
                -> pure Loop

       pure (theSubId, var, subM)
