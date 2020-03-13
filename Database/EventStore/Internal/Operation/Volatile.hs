--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.Volatile
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.Volatile (volatile) where

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
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Subscription.Message
import Database.EventStore.Internal.Subscription.Types
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
volatile
  :: Exec
  -> StreamId t
  -> Bool
  -> Maybe Credentials
  -> IO (UUID, Chan SubAction)
volatile exec streamId tos cred
  = do m <- mailboxNew
       subM <- newChan
       let req = subscribeToStream stream tos
       pkg <- createPkg subscribeToStreamCmd cred req
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
                | packageCmd respPkg == subscriptionConfirmationCmd
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
                   Loop <$ writeChan subM (Confirmed details)
                | packageCmd respPkg == streamEventAppearedCmd
                -> let Right resp = decodePkg respPkg
                       evt = newResolvedEventFromBuf $ getField $ streamResolvedEvent resp in
                   Loop <$ writeChan subM (Submit evt)
                | otherwise
                -> pure Loop

       pure (theSubId, subM)
  where
    stream = streamIdRaw streamId
