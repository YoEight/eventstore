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
            -> Operation (Slice EventNumber)
fetchStream setts stream batch tos cred (EventNumber n) =
    traversing go <~ readStreamEvents setts Forward stream n batch tos cred
  where
    go outcome =
        fromReadResult stream outcome pure

--------------------------------------------------------------------------------
fetchAll :: Settings
         -> Int32 -- Batch size.
         -> Bool -- Resolve link tos.
         -> Maybe Credentials
         -> Position
         -> Operation (Slice Position)
fetchAll setts batch tos cred (Position com pre) =
    traversing pure <~ readAllEvents setts com pre batch tos Forward cred

--------------------------------------------------------------------------------
sourceStream :: (t -> Operation (Slice t))
             -> t
             -> Operation SubAction
sourceStream fetch start = unfolding go
  where
    go Nothing =
        pure (fetch start)
    go (Just s) = do
        traverse_ (yield . Submit) (sliceEvents s)

        case sliceNext s of
            Just next -> pure (fetch next)
            Nothing   -> stop

--------------------------------------------------------------------------------
catchup :: forall t. Settings
        -> StreamId t
        -> t
        -> Bool        -- Resolve link tos.
        -> Maybe Int32 -- Batch size.
        -> Maybe Credentials
        -> Operation SubAction
catchup setts streamId from tos batchSiz cred =
    append (sourceStream iteratee from) (volatile streamId tos cred)
  where
    batch = fromMaybe defaultBatchSize batchSiz

    iteratee :: t -> Operation (Slice t)
    iteratee =
        case streamId of
            StreamName n -> fetchStream setts n batch tos cred
            All          -> fetchAll setts batch tos cred

--------------------------------------------------------------------------------
fromReadResult :: Text
               -> ReadResult EventNumber a
               -> (a -> Execution x)
               -> Execution x
fromReadResult stream res k =
    case res of
        ReadNoStream        -> Failed $ streamNotFound stream
        ReadStreamDeleted s -> Failed $ StreamDeleted s
        ReadNotModified     -> Failed $ ServerError Nothing
        ReadError e         -> Failed $ ServerError e
        ReadAccessDenied s  -> Failed $ AccessDenied s
        ReadSuccess ss      -> k ss
