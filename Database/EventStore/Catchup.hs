{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Catchup
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Catchup
    ( Catchup
    , CatchupError(..)
    , catchupAwait
    , catchupStart
    , catchupAllStart
    , catchupStream
    , catchupUnsubscribe
    ) where

--------------------------------------------------------------------------------
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Foldable (traverse_)
import Data.Int
import Data.Maybe
import Data.Typeable

--------------------------------------------------------------------------------
import Control.Concurrent.Async
import Data.Text

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Manager.Subscription
import Database.EventStore.Internal.Operation.ReadStreamEventsOperation
import Database.EventStore.Internal.Operation.ReadAllEventsOperation
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Errors that could arise during a catch-up subscription. 'Text' value
--   represents the stream name.
data CatchupError
    = CatchupStreamDeleted Text
    | CatchupUnexpectedStreamStatus Text ReadStreamResult
    | CatchupSubscriptionDropReason Text DropReason
    deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception CatchupError

--------------------------------------------------------------------------------
-- | Representing catch-up subscriptions.
data Catchup
    = Catchup
      { catchupStream :: Text
        -- ^ The name of the stream to which the subscription is subscribed.
      , catchupChan        :: Chan (Either CatchupError ResolvedEvent)
      , catchupUnsubscribe :: IO ()
        -- ^ Asynchronously unsubscribes from the stream.
      }

--------------------------------------------------------------------------------
-- | Awaits for the next 'ResolvedEvent'.
catchupAwait :: Catchup -> IO (Either CatchupError ResolvedEvent)
catchupAwait c = readChan $ catchupChan c

--------------------------------------------------------------------------------
defaultBatchSize :: Int32
defaultBatchSize = 500

--------------------------------------------------------------------------------
catchupStart :: (Int32 -> Int32 -> IO (Async StreamEventsSlice))
             -> IO (Async Subscription)
             -> Text
             -> Maybe Int32
             -> Maybe Int32
             -> IO Catchup
catchupStart evt_fwd get_sub stream_id batch_size_m last_m = do
    chan <- newChan
    var  <- newEmptyMVar
    let batch_size   = fromMaybe defaultBatchSize batch_size_m
        nxt_read_evt = fromMaybe 0 last_m

    as <- async $ do
        res_m <- readEventsTill evt_fwd
                                (writeChan chan)
                                stream_id
                                nxt_read_evt
                                batch_size

        maybe (return ()) throwIO res_m
        action <- get_sub
        sub    <- wait action
        putMVar var sub
        keepAwaitingSubEvent stream_id chan sub

    let catchup = Catchup
                  { catchupStream      = stream_id
                  , catchupChan        = chan
                  , catchupUnsubscribe = do
                      cancel as
                      sub_m <- tryTakeMVar var
                      traverse_ subUnsubscribe sub_m
                  }

    return catchup

--------------------------------------------------------------------------------
catchupAllStart :: ( Position -> Int32 -> IO (Async AllEventsSlice))
                -> IO (Async Subscription)
                -> Maybe Position
                -> Maybe Int32
                -> IO Catchup
catchupAllStart evt_fwd get_sub last_chk_pt_m batch_size_m = do
    chan <- newChan
    var  <- newEmptyMVar
    let batch_size = fromMaybe defaultBatchSize batch_size_m
        start_pos  = fromMaybe positionStart last_chk_pt_m

    as <- async $ do
        res_m <- readAllTill evt_fwd
                             (writeChan chan)
                             start_pos
                             batch_size

        maybe (return ()) throwIO res_m
        action <- get_sub
        sub    <- wait action
        putMVar var sub
        keepAwaitingSubEvent "" chan sub

    let catchup = Catchup
                  { catchupStream      = ""
                  , catchupChan        = chan
                  , catchupUnsubscribe = do
                      cancel as
                      sub_m <- tryTakeMVar var
                      traverse_ subUnsubscribe sub_m
                  }

    return catchup

--------------------------------------------------------------------------------
readEventsTill :: (Int32 -> Int32 -> IO (Async StreamEventsSlice))
               -> (Either CatchupError ResolvedEvent -> IO ())
               -> Text
               -> Int32
               -> Int32
               -> IO (Maybe CatchupError)
readEventsTill evts_fwd proc_evt stream_id start batch_size =
    loop False start
  where
    loop done cur_evt_num
        | done      = return Nothing
        | otherwise = do
              action <- evts_fwd cur_evt_num batch_size
              slice  <- wait action
              case streamEventsSliceResult slice of
                  RS_SUCCESS -> do
                      let nxt    = streamEventsSliceNext slice
                          n_done = streamEventsSliceIsEOS slice
                          evts   = streamEventsSliceEvents slice

                      traverse_ (proc_evt . Right) evts
                      loop n_done nxt
                  RS_NO_STREAM      -> loop True cur_evt_num
                  RS_STREAM_DELETED -> reportError deletedError
                  s -> reportError $ unexpectedError s

    deletedError = CatchupStreamDeleted stream_id

    unexpectedError s = CatchupUnexpectedStreamStatus stream_id s

    reportError e = do
        proc_evt $ Left e
        return $ Just e

--------------------------------------------------------------------------------
readAllTill :: (Position -> Int32 -> IO (Async AllEventsSlice))
            -> (Either CatchupError ResolvedEvent -> IO ())
            -> Position
            -> Int32
            -> IO (Maybe CatchupError)
readAllTill evts_fwd proc_evt start batch_size =
    loop False start
  where
    loop done pos
        | done      = return Nothing
        | otherwise = do
              action <- evts_fwd pos batch_size
              slice  <- wait action
              let evts   = allEventsSliceEvents slice
                  nxt    = allEventsSliceNext slice
                  n_done = allEventsSliceIsEOS slice

              traverse_ (proc_evt . Right) evts
              loop n_done nxt

--------------------------------------------------------------------------------
keepAwaitingSubEvent :: Text
                     -> Chan (Either CatchupError ResolvedEvent)
                     -> Subscription
                     -> IO ()
keepAwaitingSubEvent stream_id chan sub = forever $ do
    evt_e <- subAwait sub
    case evt_e of
        Right evt -> writeChan chan (Right evt)
        Left r    -> do
            let e = CatchupSubscriptionDropReason stream_id r

            writeChan chan (Left e)
            throwIO e
