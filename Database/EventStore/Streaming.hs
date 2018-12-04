{-# LANGUAGE LambdaCase #-}
--------------------------------------------------------------------------------
-- |
-- Module    :  Database.EventStore.Streaming
-- Copyright :  (C) 2018 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Streaming
    ( ReadException(..)
    , readStreamThroughForward
    , readStreamThroughBackward
    , readAllThroughForward
    , readAllThroughBackward
    ) where

--------------------------------------------------------------------------------
import Control.Exception (Exception, throwIO)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Prelude

--------------------------------------------------------------------------------
import Data.Text (Text)

--------------------------------------------------------------------------------
import           Control.Concurrent.Async.Lifted (wait)
import qualified Database.EventStore as ES
import           Database.EventStore.Internal.Operation.Read.Common (emptySlice)
import           Database.EventStore.Internal.Types (EventNumber(..))
import           Streaming
import qualified Streaming.Prelude as Streaming

--------------------------------------------------------------------------------
data ReadException
    = StreamDeleted !ES.StreamName
    | ReadError !(Maybe Text)
    | AccessDenied !ES.StreamName
    deriving Show

--------------------------------------------------------------------------------
instance Exception ReadException

--------------------------------------------------------------------------------
data Fetch t = FetchError !ReadException | Fetch !(ES.Slice t)

--------------------------------------------------------------------------------
toFetch :: ES.ReadResult s (ES.Slice t) -> Fetch t
toFetch ES.ReadNoStream          = Fetch ES.emptySlice
toFetch ES.ReadNotModified       = Fetch ES.emptySlice
toFetch (ES.ReadStreamDeleted n) = FetchError (StreamDeleted n)
toFetch (ES.ReadError e)         = FetchError (ReadError e)
toFetch (ES.ReadAccessDenied n)  = FetchError (AccessDenied n)
toFetch (ES.ReadSuccess s)       = Fetch s

--------------------------------------------------------------------------------
data State t = Need t | Fetched ![ES.ResolvedEvent] !(Maybe t)

--------------------------------------------------------------------------------
streaming :: (t -> IO (Fetch t)) -> t -> Stream (Of ES.ResolvedEvent) IO ()
streaming iteratee = Streaming.unfoldr go . Need
  where
      go (Fetched buffer next) =
          case buffer of
            e:rest -> pure (Right (e, Fetched rest next))
            []     -> maybe stop (go . Need) next
      go (Need pos) = do
          iteratee pos >>= \case
              FetchError e -> throwIO e
              Fetch s      ->
                  case s of
                      ES.SliceEndOfStream -> stop
                      ES.Slice xs next    -> go (Fetched xs next)

      stop = pure (Left ())

--------------------------------------------------------------------------------
-- | Returns an iterator able to consume a stream entirely. When reading forward,
--   the iterator ends when the last stream's event is reached.
readStreamThroughForward :: ES.Connection
                         -> ES.StreamName
                         -> ES.ResolveLink
                         -> ES.EventNumber
                         -> Maybe Int32
                         -> Maybe ES.Credentials
                         -> Stream (Of ES.ResolvedEvent) IO ()
readStreamThroughForward conn = readStreamThrough conn ES.Forward

--------------------------------------------------------------------------------
-- | Returns an iterator able to consume a stream entirely. When reading backward,
--   the iterator ends when the first stream's event is reached.
readStreamThroughBackward :: ES.Connection
                          -> ES.StreamName
                          -> ES.ResolveLink
                          -> ES.EventNumber
                          -> Maybe Int32
                          -> Maybe ES.Credentials
                          -> Stream (Of ES.ResolvedEvent) IO ()
readStreamThroughBackward conn = readStreamThrough conn ES.Backward

--------------------------------------------------------------------------------
readStreamThrough :: ES.Connection
                  -> ES.ReadDirection
                  -> ES.StreamName
                  -> ES.ResolveLink
                  -> ES.EventNumber
                  -> Maybe Int32
                  -> Maybe ES.Credentials
                  -> Stream (Of ES.ResolvedEvent) IO ()
readStreamThrough conn dir name lnk from sizMay cred = streaming iteratee from
  where
    batchSize = fromMaybe 500 sizMay

    iteratee =
        case dir of
            ES.Forward  -> readForward conn name batchSize lnk cred
            ES.Backward -> readBackward conn name batchSize lnk cred

--------------------------------------------------------------------------------
-- | Returns an iterator able to consume $all stream entirely. When reading forward,
--   the iterator ends when the last stream's event is reached.
readAllThroughForward :: ES.Connection
                      -> ES.ResolveLink
                      -> ES.Position
                      -> Maybe Int32
                      -> Maybe ES.Credentials
                      -> Stream (Of ES.ResolvedEvent) IO ()
readAllThroughForward conn = readAllThrough conn ES.Forward

--------------------------------------------------------------------------------
-- | Returns an iterator able to consume $all stream entirely. When reading backward,
--   the iterator ends when the first stream's event is reached.
readAllThroughBackward :: ES.Connection
                       -> ES.ResolveLink
                       -> ES.Position
                       -> Maybe Int32
                       -> Maybe ES.Credentials
                       -> Stream (Of ES.ResolvedEvent) IO ()
readAllThroughBackward conn = readAllThrough conn ES.Backward

--------------------------------------------------------------------------------
readAllThrough :: ES.Connection
               -> ES.ReadDirection
               -> ES.ResolveLink
               -> ES.Position
               -> Maybe Int32
               -> Maybe ES.Credentials
               -> Stream (Of ES.ResolvedEvent) IO ()
readAllThrough conn dir lnk from sizMay creds = streaming iteratee from
  where
    batchSize = fromMaybe 500 sizMay

    iteratee =
        case dir of
            ES.Forward  -> readAllForward conn batchSize lnk creds
            ES.Backward -> readAllBackward conn batchSize lnk creds

--------------------------------------------------------------------------------
readForward :: ES.Connection
            -> ES.StreamName
            -> Int32
            -> ES.ResolveLink
            -> Maybe ES.Credentials
            -> ES.EventNumber
            -> IO (Fetch EventNumber)
readForward conn name siz lnk creds start =
    fmap toFetch . wait =<<
        ES.readStreamEventsForward conn name start siz lnk creds

--------------------------------------------------------------------------------
readBackward :: ES.Connection
             -> ES.StreamName
             -> Int32
             -> ES.ResolveLink
             -> Maybe ES.Credentials
             -> ES.EventNumber
             -> IO (Fetch EventNumber)
readBackward conn name siz lnk creds start =
    fmap toFetch . wait =<<
        ES.readStreamEventsBackward conn name start siz lnk creds

--------------------------------------------------------------------------------
readAllForward :: ES.Connection
               -> Int32
               -> ES.ResolveLink
               -> Maybe ES.Credentials
               -> ES.Position
               -> IO (Fetch ES.Position)
readAllForward conn siz lnk creds start =
    fmap Fetch . wait =<<
        ES.readAllEventsForward conn start siz lnk creds

--------------------------------------------------------------------------------
readAllBackward :: ES.Connection
                -> Int32
                -> ES.ResolveLink
                -> Maybe ES.Credentials
                -> ES.Position
                -> IO (Fetch ES.Position)
readAllBackward conn siz lnk creds start =
    fmap Fetch . wait =<<
        ES.readAllEventsBackward conn start siz lnk creds
