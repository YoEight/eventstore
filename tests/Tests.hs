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
