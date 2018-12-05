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

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Operation.Read.Common
import Database.EventStore.Internal.Operation.ReadAllEvents
import Database.EventStore.Internal.Operation.ReadStreamEvents
import Database.EventStore.Internal.Operation.Volatile
import Database.EventStore.Internal.Prelude
import Database.EventStore.Internal.Settings
import Database.EventStore.Internal.Stream
import Database.EventStore.Internal.Subscription.Types
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
defaultBatchSize :: Int32
defaultBatchSize = 500

--------------------------------------------------------------------------------
streamNotFound :: Text -> OperationError
streamNotFound stream = StreamNotFound $ StreamName stream

--------------------------------------------------------------------------------
fetchStream :: Settings
            -> Text -- Stream name.
            -> Int32 -- Batch size.
            -> Bool -- Resolve link tos.
            -> Maybe Credentials
            -> EventNumber
            -> Code o (Slice EventNumber)
fetchStream setts stream batch tos cred (EventNumber n) = do
    outcome <-
        deconstruct $ fmap Left $
            readStreamEvents setts Forward stream n batch tos cred

    fromReadResult stream outcome pure

--------------------------------------------------------------------------------
fetchAll :: Settings
         -> Int32 -- Batch size.
         -> Bool -- Resolve link tos.
         -> Maybe Credentials
         -> Position
         -> Code o (Slice Position)
fetchAll setts batch tos cred (Position com pre) =
    deconstruct $ fmap Left $
        readAllEvents setts com pre batch tos Forward cred

--------------------------------------------------------------------------------
sourceStream :: t
             -> (forall o. t -> Code o (Slice t))
             -> Operation SubAction
sourceStream seed iteratee = unfoldPlan seed go
  where
    go state = do
        s <- iteratee state
        traverse_ (yield . Submit) (sliceEvents s)

        case sliceNext s of
            Just newState -> pure newState
            Nothing       -> stop

--------------------------------------------------------------------------------
catchup :: forall t. Settings
        -> StreamId t
        -> t
        -> Bool        -- Resolve link tos.
        -> Maybe Int32 -- Batch size.
        -> Maybe Credentials
        -> Operation SubAction
catchup setts streamId from tos batchSiz cred =
    sourceStream from iteratee <> volatile streamId tos cred
  where
    batch = fromMaybe defaultBatchSize batchSiz

    iteratee :: t -> Code o (Slice t)
    iteratee =
        case streamId of
            StreamName n -> fetchStream setts n batch tos cred
            All          -> fetchAll setts batch tos cred

--------------------------------------------------------------------------------
fromReadResult :: Text
               -> ReadResult EventNumber a
               -> (a -> Code o x)
               -> Code o x
fromReadResult stream res k =
    case res of
        ReadNoStream        -> failure $ streamNotFound stream
        ReadStreamDeleted s -> failure $ StreamDeleted s
        ReadNotModified     -> failure $ ServerError Nothing
        ReadError e         -> failure $ ServerError e
        ReadAccessDenied s  -> failure $ AccessDenied s
        ReadSuccess ss      -> k ss
