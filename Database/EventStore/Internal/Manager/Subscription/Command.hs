--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Manager.Subscription.Command
-- Copyright : (C) 2016 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Manager.Subscription.Command where

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.ProtocolBuffers
import Data.Serialize

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Manager.Subscription.Message
import Database.EventStore.Internal.Manager.Subscription.Types
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data ServerMessage
    = EventAppearedMsg !ResolvedEvent
    | PersistentEventAppearedMsg !ResolvedEvent
    | ConfirmationMsg !Int64 !(Maybe Int32)
    | PersistentConfirmationMsg !Text !Int64 !(Maybe Int32)
    | PersistentCreatedMsg CreatePersistentSubscriptionResult
    | PersistentUpdatedMsg UpdatePersistentSubscriptionResult
    | PersistentDeletedMsg DeletePersistentSubscriptionResult
    | DroppedMsg SubDropReason
    | BadRequestMsg !(Maybe Text)
    | NotHandledMsg !NotHandledReason !(Maybe MasterInfo)
    | NotAuthenticatedMsg !(Maybe Text)
    | UnknownMsg !(Maybe Command)

--------------------------------------------------------------------------------
toSubDropReason :: DropReason -> SubDropReason
toSubDropReason D_Unsubscribed                  = SubUnsubscribed
toSubDropReason D_NotFound                      = SubNotFound
toSubDropReason D_AccessDenied                  = SubAccessDenied
toSubDropReason D_PersistentSubscriptionDeleted = SubPersistDeleted
toSubDropReason D_SubscriberMaxCountReached     = SubSubscriberMaxCountReached

--------------------------------------------------------------------------------
decodeServerMessage :: Package -> ServerMessage
decodeServerMessage pkg = fromMaybe (UnknownMsg $ Just (packageCmd pkg)) go
  where
    go =
        case packageCmd pkg of
            cmd | cmd == streamEventAppearedCmd -> do
                msg <- maybeDecodeMessage $ packageData pkg
                let evt = newResolvedEventFromBuf $ getField
                                                  $ streamResolvedEvent msg
                return $ EventAppearedMsg evt
                | cmd == persistentSubscriptionStreamEventAppearedCmd -> do
                msg <- maybeDecodeMessage $ packageData pkg
                let evt = newResolvedEvent $ getField $ psseaEvt msg
                return $ PersistentEventAppearedMsg evt
                | cmd == subscriptionConfirmationCmd -> do
                msg <- maybeDecodeMessage $ packageData pkg
                let lcp = getField $ subscribeLastCommitPos msg
                    len = getField $ subscribeLastEventNumber msg
                return $ ConfirmationMsg lcp len
                | cmd == persistentSubscriptionConfirmationCmd -> do
                msg <- maybeDecodeMessage $ packageData pkg
                let lcp = getField $ pscLastCommitPos msg
                    sid = getField $ pscId msg
                    len = getField $ pscLastEvtNumber msg
                return $ PersistentConfirmationMsg sid lcp len
                | cmd == createPersistentSubscriptionCompletedCmd ->
                fmap (PersistentCreatedMsg . getField . cpscResult)
                    $ maybeDecodeMessage
                    $ packageData pkg
                | cmd == updatePersistentSubscriptionCompletedCmd ->
                fmap (PersistentUpdatedMsg . getField . upscResult)
                    $ maybeDecodeMessage
                    $ packageData pkg
                | cmd == deletePersistentSubscriptionCompletedCmd ->
                fmap (PersistentDeletedMsg . getField . dpscResult)
                    $ maybeDecodeMessage
                    $ packageData pkg
                | cmd == subscriptionDroppedCmd -> do
                msg <- maybeDecodeMessage $ packageData pkg
                let reason = fromMaybe D_Unsubscribed $ getField
                                                      $ dropReason msg
                return $ DroppedMsg $ toSubDropReason reason
                | cmd == badRequestCmd ->
                return $ BadRequestMsg $ packageDataAsText pkg
                | cmd == notAuthenticatedCmd ->
                return $ NotAuthenticatedMsg $ packageDataAsText pkg
                | cmd == notHandledCmd -> do
                msg <- maybeDecodeMessage $ packageData pkg
                let info = fmap masterInfo $ getField
                                           $ notHandledAdditionalInfo msg
                    reason = getField $ notHandledReason msg
                return $ NotHandledMsg reason info
                | otherwise -> Nothing

--------------------------------------------------------------------------------
maybeDecodeMessage :: Decode a => ByteString -> Maybe a
maybeDecodeMessage bytes =
    case runGet decodeMessage bytes of
        Right a -> Just a
        _       -> Nothing
