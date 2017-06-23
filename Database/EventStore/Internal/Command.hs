{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Command
-- Copyright : (C) 2016 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Command where

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Utils

--------------------------------------------------------------------------------
-- | Internal command representation.
data Command =
    Command { cmdWord8 :: !Word8
            , cmdDesc  :: !Text
            }

--------------------------------------------------------------------------------
instance Show Command where
    show c = "(" <> prettyWord8 (cmdWord8 c) <> ")" <> unpack (cmdDesc c)

--------------------------------------------------------------------------------
instance Eq Command where
    c1 == c2 = cmdWord8 c1 == cmdWord8 c2

--------------------------------------------------------------------------------
instance Ord Command where
    compare c1 c2 = compare (cmdWord8 c1) (cmdWord8 c2)

--------------------------------------------------------------------------------
heartbeatRequestCmd :: Command
heartbeatRequestCmd =
    Command { cmdWord8 = 0x01
            , cmdDesc  = "[heartbeat-request]"
            }

--------------------------------------------------------------------------------
heartbeatResponseCmd :: Command
heartbeatResponseCmd =
    Command { cmdWord8 = 0x02
            , cmdDesc  = "[heartbeat-response]"
            }

--------------------------------------------------------------------------------
writeEventsCmd :: Command
writeEventsCmd =
    Command { cmdWord8 = 0x82
            , cmdDesc  = "[write-events]"
            }

--------------------------------------------------------------------------------
writeEventsCompletedCmd :: Command
writeEventsCompletedCmd =
    Command { cmdWord8 = 0x83
            , cmdDesc  = "[write-events-completed]"
            }

--------------------------------------------------------------------------------
transactionStartCmd :: Command
transactionStartCmd =
    Command { cmdWord8 = 0x84
            , cmdDesc  = "[transaction-start]"
            }

--------------------------------------------------------------------------------
transactionStartCompletedCmd :: Command
transactionStartCompletedCmd =
    Command { cmdWord8 = 0x85
            , cmdDesc  = "[transaction-start-completed]"
            }

--------------------------------------------------------------------------------
transactionWriteCmd :: Command
transactionWriteCmd =
    Command { cmdWord8 = 0x86
            , cmdDesc  = "[transaction-write]"
            }

--------------------------------------------------------------------------------
transactionWriteCompletedCmd :: Command
transactionWriteCompletedCmd =
    Command { cmdWord8 = 0x87
            , cmdDesc  = "[transaction-write-completed]"
            }

--------------------------------------------------------------------------------
transactionCommitCmd :: Command
transactionCommitCmd =
    Command { cmdWord8 = 0x88
            , cmdDesc  = "[transaction-commit]"
            }

--------------------------------------------------------------------------------
transactionCommitCompletedCmd :: Command
transactionCommitCompletedCmd =
    Command { cmdWord8 = 0x89
            , cmdDesc  = "[transaction-commit-completed]"
            }

--------------------------------------------------------------------------------
deleteStreamCmd :: Command
deleteStreamCmd =
    Command { cmdWord8 = 0x8A
            , cmdDesc  = "[delete-stream]"
            }

--------------------------------------------------------------------------------
deleteStreamCompletedCmd :: Command
deleteStreamCompletedCmd =
    Command { cmdWord8 = 0x8B
            , cmdDesc  = "[delete-stream-completed]"
            }

--------------------------------------------------------------------------------
readEventCmd :: Command
readEventCmd =
    Command { cmdWord8 = 0xB0
            , cmdDesc  = "[read-event]"
            }

--------------------------------------------------------------------------------
readEventCompletedCmd :: Command
readEventCompletedCmd =
    Command { cmdWord8 = 0xB1
            , cmdDesc  = "[read-event-completed]"
            }

--------------------------------------------------------------------------------
readStreamEventsForwardCmd :: Command
readStreamEventsForwardCmd =
    Command { cmdWord8 = 0xB2
            , cmdDesc  = "[read-stream-events-forward]"
            }

--------------------------------------------------------------------------------
readStreamEventsForwardCompletedCmd :: Command
readStreamEventsForwardCompletedCmd =
    Command { cmdWord8 = 0xB3
            , cmdDesc  = "[read-stream-events-forward-completed]"
            }

--------------------------------------------------------------------------------
readStreamEventsBackwardCmd :: Command
readStreamEventsBackwardCmd =
    Command { cmdWord8 = 0xB4
            , cmdDesc  = "[read-stream-events-backward]"
            }

--------------------------------------------------------------------------------
readStreamEventsBackwardCompletedCmd :: Command
readStreamEventsBackwardCompletedCmd =
    Command { cmdWord8 = 0xB5
            , cmdDesc  = "[read-stream-events-backward]"
            }

--------------------------------------------------------------------------------
readAllEventsForwardCmd :: Command
readAllEventsForwardCmd =
    Command { cmdWord8 = 0xB6
            , cmdDesc  = "[read-all-events-forward]"
            }

--------------------------------------------------------------------------------
readAllEventsForwardCompletedCmd :: Command
readAllEventsForwardCompletedCmd =
    Command { cmdWord8 = 0xB7
            , cmdDesc  = "[read-all-events-forward-completed]"
            }

--------------------------------------------------------------------------------
readAllEventsBackwardCmd :: Command
readAllEventsBackwardCmd =
    Command { cmdWord8 = 0xB8
            , cmdDesc  = "[read-all-events-backward]"
            }

--------------------------------------------------------------------------------
readAllEventsBackwardCompletedCmd :: Command
readAllEventsBackwardCompletedCmd =
    Command { cmdWord8 = 0xB9
            , cmdDesc  = "[read-all-events-backward-completed]"
            }

--------------------------------------------------------------------------------
subscribeToStreamCmd :: Command
subscribeToStreamCmd =
    Command { cmdWord8 = 0xC0
            , cmdDesc  = "[subscribe-to-stream]"
            }

--------------------------------------------------------------------------------
subscriptionConfirmationCmd :: Command
subscriptionConfirmationCmd =
    Command { cmdWord8 = 0xC1
            , cmdDesc  = "[subscription-confirmation]"
            }

--------------------------------------------------------------------------------
streamEventAppearedCmd :: Command
streamEventAppearedCmd =
    Command { cmdWord8 = 0xC2
            , cmdDesc  = "[stream-event-appeared]"
            }

--------------------------------------------------------------------------------
unsubscribeFromStreamCmd :: Command
unsubscribeFromStreamCmd =
    Command { cmdWord8 = 0xC3
            , cmdDesc  = "[unsubscribe-from-stream]"
            }

--------------------------------------------------------------------------------
subscriptionDroppedCmd :: Command
subscriptionDroppedCmd =
    Command { cmdWord8 = 0xC4
            , cmdDesc  = "[subscription-dropped]"
            }

--------------------------------------------------------------------------------
connectToPersistentSubscriptionCmd :: Command
connectToPersistentSubscriptionCmd =
    Command { cmdWord8 = 0xC5
            , cmdDesc  = "[connect-to-persistent-subscription]"
            }

--------------------------------------------------------------------------------
persistentSubscriptionConfirmationCmd :: Command
persistentSubscriptionConfirmationCmd =
    Command { cmdWord8 = 0xC6
            , cmdDesc  = "[persistent-subscription-confirmation]"
            }

--------------------------------------------------------------------------------
persistentSubscriptionStreamEventAppearedCmd :: Command
persistentSubscriptionStreamEventAppearedCmd =
    Command { cmdWord8 = 0xC7
            , cmdDesc  = "[persistent-subscription-stream-event-appeared]"
            }

--------------------------------------------------------------------------------
createPersistentSubscriptionCmd :: Command
createPersistentSubscriptionCmd =
    Command { cmdWord8 = 0xC8
            , cmdDesc  = "[create-persistent-subscription]"
            }

--------------------------------------------------------------------------------
createPersistentSubscriptionCompletedCmd :: Command
createPersistentSubscriptionCompletedCmd =
    Command { cmdWord8 = 0xC9
            , cmdDesc  = "[create-persistent-subscription-completed]"
            }

--------------------------------------------------------------------------------
deletePersistentSubscriptionCmd :: Command
deletePersistentSubscriptionCmd =
    Command { cmdWord8 = 0xCA
            , cmdDesc  = "[delete-persistent-subscription]"
            }

--------------------------------------------------------------------------------
deletePersistentSubscriptionCompletedCmd :: Command
deletePersistentSubscriptionCompletedCmd =
    Command { cmdWord8 = 0xCB
            , cmdDesc  = "[delete-persistent-subscription-completed]"
            }

--------------------------------------------------------------------------------
persistentSubscriptionAckEventsCmd :: Command
persistentSubscriptionAckEventsCmd =
    Command { cmdWord8 = 0xCC
            , cmdDesc  = "[persistent-subscription-ack-events]"
            }

--------------------------------------------------------------------------------
persistentSubscriptionNakEventsCmd :: Command
persistentSubscriptionNakEventsCmd =
    Command { cmdWord8 = 0xCD
            , cmdDesc  = "[persistent-subscription-nak-events]"
            }

--------------------------------------------------------------------------------
updatePersistentSubscriptionCmd :: Command
updatePersistentSubscriptionCmd =
    Command { cmdWord8 = 0xCE
            , cmdDesc  = "[update-persistent-subscription]"
            }

--------------------------------------------------------------------------------
updatePersistentSubscriptionCompletedCmd :: Command
updatePersistentSubscriptionCompletedCmd =
    Command { cmdWord8 = 0xCF
            , cmdDesc  = "[update-persistent-subscription-completed]"
            }

--------------------------------------------------------------------------------
badRequestCmd :: Command
badRequestCmd =
    Command { cmdWord8 = 0xF0
            , cmdDesc  = "[bad-request]"
            }

--------------------------------------------------------------------------------
notHandledCmd :: Command
notHandledCmd =
    Command { cmdWord8 = 0xF1
            , cmdDesc  = "[not-handled]"
            }

--------------------------------------------------------------------------------
authenticateCmd :: Command
authenticateCmd =
    Command { cmdWord8 = 0xF2
            , cmdDesc  = "[authenticate]"
            }

--------------------------------------------------------------------------------
authenticatedCmd :: Command
authenticatedCmd =
    Command { cmdWord8 = 0xF3
            , cmdDesc  = "[authenticated]"
            }

--------------------------------------------------------------------------------
notAuthenticatedCmd :: Command
notAuthenticatedCmd =
    Command { cmdWord8 = 0xF4
            , cmdDesc  = "[not-authenticated]"
            }

--------------------------------------------------------------------------------
identifyClientCmd :: Command
identifyClientCmd =
    Command { cmdWord8 = 0xF5
            , cmdDesc  = "[identify-client]"
            }

--------------------------------------------------------------------------------
clientIdentifiedCmd :: Command
clientIdentifiedCmd =
    Command { cmdWord8 = 0xF6
            , cmdDesc  = "[client-identified]"
            }

--------------------------------------------------------------------------------
unknownCmd :: Word8 -> Command
unknownCmd w =
    Command { cmdWord8 = w
            , cmdDesc  = "[unknown: "<> pack (prettyWord8 w) <> "]"
            }

--------------------------------------------------------------------------------
getCommand :: Word8 -> Command
getCommand w =
    case lookup w _cmdDict of
        Just cmd -> cmd
        Nothing  -> unknownCmd w

--------------------------------------------------------------------------------
_cmdDict :: HashMap Word8 Command
_cmdDict = mapFromList
    [ (0x01, heartbeatRequestCmd)
    , (0x02, heartbeatResponseCmd)
    , (0x82, writeEventsCmd)
    , (0x83, writeEventsCompletedCmd)
    , (0x84, transactionStartCmd)
    , (0x85, transactionStartCompletedCmd)
    , (0x86, transactionWriteCmd)
    , (0x87, transactionWriteCompletedCmd)
    , (0x88, transactionCommitCmd)
    , (0x89, transactionCommitCompletedCmd)
    , (0x8A, deleteStreamCmd)
    , (0x8B, deleteStreamCompletedCmd)
    , (0xB0, readEventCmd)
    , (0xB1, readEventCompletedCmd)
    , (0xB2, readStreamEventsForwardCmd)
    , (0xB3, readStreamEventsForwardCompletedCmd)
    , (0xB4, readStreamEventsBackwardCmd)
    , (0xB5, readStreamEventsBackwardCompletedCmd)
    , (0xB6, readAllEventsForwardCmd)
    , (0xB7, readAllEventsForwardCompletedCmd)
    , (0xB8, readAllEventsBackwardCmd)
    , (0xB9, readAllEventsBackwardCompletedCmd)
    , (0xC0, subscribeToStreamCmd)
    , (0xC1, subscriptionConfirmationCmd)
    , (0xC2, streamEventAppearedCmd)
    , (0xC3, unsubscribeFromStreamCmd)
    , (0xC4, subscriptionDroppedCmd)
    , (0xC5, connectToPersistentSubscriptionCmd)
    , (0xC6, persistentSubscriptionConfirmationCmd)
    , (0xC7, persistentSubscriptionStreamEventAppearedCmd)
    , (0xC8, createPersistentSubscriptionCmd)
    , (0xC9, createPersistentSubscriptionCompletedCmd)
    , (0xCA, deletePersistentSubscriptionCmd)
    , (0xCB, deletePersistentSubscriptionCompletedCmd)
    , (0xCC, persistentSubscriptionAckEventsCmd)
    , (0xCD, persistentSubscriptionNakEventsCmd)
    , (0xCE, updatePersistentSubscriptionCmd)
    , (0xCF, updatePersistentSubscriptionCompletedCmd)
    , (0xF0, badRequestCmd)
    , (0xF1, notHandledCmd)
    , (0xF2, authenticateCmd)
    , (0xF3, authenticatedCmd)
    , (0xF4, notAuthenticatedCmd)
    , (0xF5, identifyClientCmd)
    , (0xF6, clientIdentifiedCmd)
    ]