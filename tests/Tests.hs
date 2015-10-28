{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module : Tests
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
-- Gathers all EventStore operations tests.
--------------------------------------------------------------------------------
module Tests where

--------------------------------------------------------------------------------
import Data.Maybe (catMaybes)
import System.IO

--------------------------------------------------------------------------------
import Data.Aeson
import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
import Database.EventStore

--------------------------------------------------------------------------------
tests :: Connection -> TestTree
tests conn = testGroup "EventStore actions tests"
    [ testCase "Write event" $ writeEventTest conn
    , testCase "Read event" $ readEventTest conn
    , testCase "Delete stream" $ deleteStreamTest conn
    , testCase "Transaction" $ transactionTest conn
    , testCase "Read forward" $ readStreamEventForwardTest conn
    ]

--------------------------------------------------------------------------------
writeEventTest :: Connection -> IO ()
writeEventTest conn = do
    let js  = [ "baz" .= True ]
        evt = createEvent "foo" Nothing $ withJson js

    as <- sendEvent conn "write-event-test" anyStream evt
    _  <- wait as
    return ()

--------------------------------------------------------------------------------
readEventTest :: Connection -> IO ()
readEventTest conn = do
    let js  = [ "baz" .= True ]
        evt = createEvent "foo" Nothing $ withJson js
    as <- sendEvent conn "read-event-test" anyStream evt
    _  <- wait as
    bs <- readEvent conn "read-event-test" 0 False
    rs <- wait bs
    case rs of
        ReadSuccess re ->
            case re of
                ReadEvent _ _ revt ->
                    let action = resolvedEventOriginal revt >>=
                                 recordedEventDataAsJson in
                    case action of
                        Just js_evt ->
                            assertEqual "event should match" js js_evt
                        Nothing -> fail "Error when retrieving recorded data"
                _ -> fail "Event not found"
        e -> fail $ "Read failure: " ++ show e

--------------------------------------------------------------------------------
deleteStreamTest :: Connection -> IO ()
deleteStreamTest conn = do
    let js  = [ "baz" .= True ]
        evt = createEvent "foo" Nothing $ withJson js
    _ <- sendEvent conn "delete-stream-test" anyStream evt >>= wait
    _ <- deleteStream conn "delete-stream-test" anyStream Nothing
    return ()

--------------------------------------------------------------------------------
transactionTest :: Connection -> IO ()
transactionTest conn = do
    let js  = [ "baz" .= True ]
        evt = createEvent "foo" Nothing $ withJson js
    t  <- startTransaction conn "transaction-test" anyStream >>= wait
    _  <- transactionWrite t [evt] >>= wait
    rs <- readEvent conn "transaction-test" 0 False >>= wait
    case rs of
        ReadNoStream -> return ()
        e -> fail $ "transaction-test stream is supposed to not exist "
                  ++ show e
    _   <- transactionCommit t >>= wait
    rs2 <- readEvent conn "transaction-test" 0 False >>= wait
    case rs2 of
        ReadSuccess re ->
            case re of
                ReadEvent _ _ revt ->
                    let action = resolvedEventOriginal revt >>=
                                 recordedEventDataAsJson in
                    case action of
                        Just js_evt ->
                            assertEqual "event should match" js js_evt
                        Nothing -> fail "Error when retrieving recorded data"
                _ -> fail "Event not found"
        e -> fail $ "Read failure: " ++ show e

--------------------------------------------------------------------------------
readStreamEventForwardTest :: Connection -> IO ()
readStreamEventForwardTest conn = do
    let jss = [ [ "baz" .= True]
              , [ "foo" .= False]
              , [ "bar" .= True]
              ]
        evts = fmap (createEvent "foo" Nothing . withJson) jss
    _  <- sendEvents conn "read-forward-test" anyStream evts >>= wait
    rs <- readStreamEventsForward conn "read-forward-test" 0 10 False >>= wait
    case rs of
        ReadSuccess sl -> do
            let action e = resolvedEventOriginal e >>=
                           recordedEventDataAsJson
                jss_evts = catMaybes $ fmap action $ sliceEvents sl
            assertEqual "Events should be equal" jss jss_evts
        e -> fail $ "Read failure: " ++ show e
