--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Subscription.Command
-- Copyright : (C) 2016 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Subscription.Command where

--------------------------------------------------------------------------------
import Data.ProtocolBuffers
import Data.Serialize

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Subscription.Message
import Database.EventStore.Internal.Subscription.Types
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data ServerMessage
    = LiveMsg !LiveMsg
    | ConfirmationMsg !ConfirmationMsg
    | ErrorMsg !ErrorMsg

--------------------------------------------------------------------------------
data LiveMsg
    = EventAppearedMsg !ResolvedEvent
    | PersistentEventAppearedMsg !ResolvedEvent
    | DroppedMsg !SubDropReason

--------------------------------------------------------------------------------
data ConfirmationMsg
    = RegularConfirmationMsg !Int64 !(Maybe Int64)
    | PersistentConfirmationMsg !Text !Int64 !(Maybe Int64)

--------------------------------------------------------------------------------
confirmationCommitPos :: ConfirmationMsg -> Int64
confirmationCommitPos (RegularConfirmationMsg pos _)       = pos
confirmationCommitPos  (PersistentConfirmationMsg _ pos _) = pos

--------------------------------------------------------------------------------
confirmationLastEventNum :: ConfirmationMsg -> Maybe Int64
confirmationLastEventNum (RegularConfirmationMsg _ num)      = num
confirmationLastEventNum (PersistentConfirmationMsg _ _ num) = num

--------------------------------------------------------------------------------
confirmationPersistentSubId :: ConfirmationMsg -> Maybe Text
confirmationPersistentSubId (PersistentConfirmationMsg ident _ _) = Just ident
confirmationPersistentSubId _                                     = Nothing

--------------------------------------------------------------------------------
data ErrorMsg
    = BadRequestMsg !(Maybe Text)
    | NotHandledMsg !NotHandledReason !(Maybe MasterInfo)
    | NotAuthenticatedMsg !(Maybe Text)
    | UnknownMsg !(Maybe Command)

--------------------------------------------------------------------------------
decodeServerMessage :: Package -> ServerMessage
decodeServerMessage pkg = fromMaybe err go
  where
    err = ErrorMsg $ UnknownMsg $ Just $ packageCmd pkg
    go =
        case packageCmd pkg of
            cmd | cmd == streamEventAppearedCmd -> do
                msg <- maybeDecodeMessage $ packageData pkg
                let evt = newResolvedEventFromBuf $ getField
                                                  $ streamResolvedEvent msg
                return $ LiveMsg $ EventAppearedMsg evt
                | cmd == persistentSubscriptionStreamEventAppearedCmd -> do
                msg <- maybeDecodeMessage $ packageData pkg
                let evt = newResolvedEvent $ getField $ psseaEvt msg
                return $ LiveMsg $ PersistentEventAppearedMsg evt
                | cmd == subscriptionConfirmationCmd -> do
                msg <- maybeDecodeMessage $ packageData pkg
                let lcp = getField $ subscribeLastCommitPos msg
                    len = getField $ subscribeLastEventNumber msg
                return $ ConfirmationMsg $ RegularConfirmationMsg lcp len
                | cmd == persistentSubscriptionConfirmationCmd -> do
                msg <- maybeDecodeMessage $ packageData pkg
                let lcp = getField $ pscLastCommitPos msg
                    sid = getField $ pscId msg
                    len = getField $ pscLastEvtNumber msg
                return $ ConfirmationMsg $ PersistentConfirmationMsg sid lcp len
                | cmd == subscriptionDroppedCmd -> do
                msg <- maybeDecodeMessage $ packageData pkg
                let reason = fromMaybe D_Unsubscribed $ getField
                                                      $ dropReason msg
                return $ LiveMsg $ DroppedMsg $ toSubDropReason reason
                | cmd == badRequestCmd ->
                return $ ErrorMsg $ BadRequestMsg $ packageDataAsText pkg
                | cmd == notAuthenticatedCmd ->
                return $ ErrorMsg $ NotAuthenticatedMsg $ packageDataAsText pkg
                | cmd == notHandledCmd -> do
                msg <- maybeDecodeMessage $ packageData pkg
                let info = fmap masterInfo $ getField
                                           $ notHandledAdditionalInfo msg
                    reason = getField $ notHandledReason msg
                return $ ErrorMsg $ NotHandledMsg reason info
                | otherwise -> Nothing

--------------------------------------------------------------------------------
maybeDecodeMessage :: Decode a => ByteString -> Maybe a
maybeDecodeMessage bytes =
    case runGet decodeMessage bytes of
        Right a -> Just a
        _       -> Nothing
