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
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Settings
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Subscription.Message
import Database.EventStore.Internal.Subscription.Types
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
volatile :: StreamId t -> Bool -> Maybe Credentials -> Operation SubAction
volatile streamId tos cred = construct (issueRequest stream tos cred)
  where
    stream = streamIdRaw streamId

--------------------------------------------------------------------------------
issueRequest :: Text -> Bool -> Maybe Credentials -> Code SubAction ()
issueRequest stream tos cred = do
  let req = subscribeToStream stream tos
  request subscribeToStreamCmd cred req
    [ Expect subscriptionDroppedCmd $ \_ d ->
        handleDropped d
    , Expect subscriptionConfirmationCmd $ \sid c -> do
        let lcp     = getField $ subscribeLastCommitPos c
            len     = getField $ subscribeLastEventNumber c
            details =
              SubDetails
              { subId           = sid
              , subCommitPos    = lcp
              , subLastEventNum = len
              , subSubId        = Nothing
              }
        yield (Confirmed details)
        live sid
    ]

--------------------------------------------------------------------------------
eventAppeared :: StreamEventAppeared -> Code SubAction ()
eventAppeared e = do
  let evt = newResolvedEventFromBuf $ getField $ streamResolvedEvent e
  yield (Submit evt)

--------------------------------------------------------------------------------
live :: UUID -> Code SubAction ()
live subscriptionId = loop
  where
    loop =
      waitForOr subscriptionId connectionReset
        [ Expect subscriptionDroppedCmd $ \_ d ->
            handleDropped d
        , Expect streamEventAppearedCmd $ \_ e -> do
            eventAppeared e
            loop
        ]

--------------------------------------------------------------------------------
connectionReset :: Code SubAction ()
connectionReset = yield ConnectionReset

--------------------------------------------------------------------------------
handleDropped :: SubscriptionDropped -> Code SubAction ()
handleDropped d = do
  let reason = fromMaybe D_Unsubscribed (getField $ dropReason d)
  yield (Dropped $ toSubDropReason reason)
