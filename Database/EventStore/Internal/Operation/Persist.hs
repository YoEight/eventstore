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
import ClassyPrelude
import Data.ProtocolBuffers
import Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Subscription.Message
import Database.EventStore.Internal.Subscription.Types
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
persist :: Text -> Text -> Int32 -> Operation SubAction
persist grp stream bufSize = construct (issueRequest grp stream bufSize)

--------------------------------------------------------------------------------
issueRequest :: Text -> Text -> Int32 -> Code SubAction ()
issueRequest grp stream bufSize = do
  let req = _connectToPersistentSubscription grp stream bufSize
  request connectToPersistentSubscriptionCmd req
    [ Expect subscriptionDroppedCmd $ \_ d ->
        handleDropped d
    , Expect persistentSubscriptionConfirmationCmd $ \sid c -> do
        let lcp      = getField $ pscLastCommitPos c
            subSubId = getField $ pscId c
            len      = getField $ pscLastEvtNumber c
            details  =
              SubDetails
              { subId           = sid
              , subCommitPos    = lcp
              , subLastEventNum = len
              , subSubId        = Just subSubId
              }
        yield (Confirmed details)
        live sid
    ]

--------------------------------------------------------------------------------
eventAppeared :: PersistentSubscriptionStreamEventAppeared -> Code SubAction ()
eventAppeared e = do
  let evt = newResolvedEvent $ getField $ psseaEvt e
  yield (Submit evt)

--------------------------------------------------------------------------------
live :: UUID -> Code SubAction ()
live subscriptionId = loop
  where
    loop =
      waitFor subscriptionId
        [ Expect subscriptionDroppedCmd $ \_ d ->
            handleDropped d
        , Expect persistentSubscriptionStreamEventAppearedCmd $ \_ e -> do
            eventAppeared e
            loop
        ]

--------------------------------------------------------------------------------
handleDropped :: SubscriptionDropped -> Code SubAction ()
handleDropped d = do
  let reason = fromMaybe D_Unsubscribed (getField $ dropReason d)
  yield (Dropped $ toSubDropReason reason)