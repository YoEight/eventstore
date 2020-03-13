{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
--------------------------------------------------------------------------------
-- |
-- Module : Test.Integration.Tests
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
-- Gathers all EventStore operations tests.
--------------------------------------------------------------------------------
module Test.Integration.Tests (spec) where

--------------------------------------------------------------------------------
import Prelude (fail)
import Control.Concurrent.Async (wait)
import Data.Aeson
import Data.DotNet.TimeSpan
import Data.FileEmbed (embedFile)
import Data.Maybe (fromMaybe)
import Data.UUID hiding (null)
import Data.UUID.V4
import qualified Streaming.Prelude as Streaming
import System.Environment (lookupEnv)
import Test.Tasty.HUnit
import Test.Tasty.Hspec

--------------------------------------------------------------------------------
import Database.EventStore
    ( SubscriptionClosed(..)
    , ReadResult(..)
    , StreamId(..)
    , StreamName
    , Slice(..)
    , ConnectionType(..)
    , Connection
    , SubAction(..)
    , acknowledge
    , getStreamMetadata
    , sendEvent
    , sendEvents
    , setStreamMetadata
    , nextSubEvent
    , streamSubEvents
    , connectToPersistentSubscription
    , unsubscribe
    , createPersistentSubscription
    , deletePersistentSubscription
    , updatePersistentSubscription
    , subscribeFrom
    , readEventsBackward
    , readEventsForward
    , subscribe
    , connect
    , waitTillClosed
    , shutdown
    , readEvent
    , startTransaction
    , transactionWrite
    , deleteStream
    , transactionCommit
    , sliceEvents
    )
import Database.EventStore.Internal.Test hiding
    ( Connection(..)
    , subscribe
    , wait
    , connect
    , readEvent
    , transactionWrite
    , deleteStream
    , transactionCommit
    , i
    )
import Data.Maybe (fromJust)
import Database.EventStore.Streaming
import qualified Streaming.Prelude as Streaming
import Test.Common

--------------------------------------------------------------------------------
createConnection :: IO Connection
createConnection = do
    hostm <- lookupEnv "EVENTSTORE_HOST"
    let host  = fromMaybe "127.0.0.1" hostm
    let setts = testSettings
                { s_defaultUserCredentials = Just $ credentials "admin" "changeit"
                , s_reconnect_delay        = 3
                }

    connect setts (Static host 1113)

--------------------------------------------------------------------------------
shuttingDown :: Connection -> IO ()
shuttingDown conn = do
    shutdown conn
    waitTillClosed conn

--------------------------------------------------------------------------------
spec :: Spec
spec = beforeAll createConnection $ afterAll shuttingDown $ describe "Features" $ parallel $ do
    it "writes events" writeEventTest
    it "reads events" readEventTest
    it "deletes stream" deleteStreamTest
    it "uses transactions" transactionTest
    it "reads forward" readStreamEventForwardTest
    it "reads backward" readStreamEventBackwardTest
    it "reads $all forward" readAllEventsForwardTest
    it "reads $all backward" readAllEventsBackwardTest
    it "creates a volatile subscription" subscribeTest
    it "creates a catchup subscription" subscribeFromTest
    it "proves catchup subscriptions don't deadlock on non existant streams" subscribeFromNoStreamTest
    it "parses empty stream acl" emptyAclParsing
    it "parses single ACL property" aclSingleReadParsing
    it "parses multiple ACL property" aclMultipleReadParsing
    it "parses empty stream meta" emptyStreamMetaParsing
    it "parses stream meta property" metaPropParsing
    it "sets stream metadata" setStreamMetadataTest
    it "gets stream metadata" getStreamMetadataTest
    it "creates a persistent subscription" createPersistentTest
    it "updates a persistent subscription"  updatePersistentTest
    it "deletes a persistent subscription" deletePersistentTest
    it "connects to a persistent subscription" connectToPersistentTest
    it "set MaxAge metadata correctly" maxAgeTest
    it "streams regular stream (forward)" streamRegularStreamForwardTest
    it "streams regular stream (backward)" streamRegularStreamBackwardTest

--------------------------------------------------------------------------------
freshStreamId :: IO StreamName
freshStreamId = fmap (StreamName . toText) nextRandom

--------------------------------------------------------------------------------
writeEventTest :: Connection -> IO ()
writeEventTest conn = do
    let js  = object [ "baz" .= True ]
        evt = createEvent "foo" Nothing $ withJson js

    stream <- freshStreamId
    as <- sendEvent conn stream anyVersion evt Nothing
    _  <- wait as
    return ()

--------------------------------------------------------------------------------
readEventTest :: Connection -> IO ()
readEventTest conn = do
    stream <- freshStreamId

    let js  = object [ "baz" .= True ]
        evt = createEvent "foo" Nothing $ withJson js
    as <- sendEvent conn stream anyVersion evt Nothing
    _  <- wait as
    bs <- readEvent conn stream streamStart NoResolveLink Nothing
    rs <- wait bs
    case rs of
        ReadSuccess re ->
            case re of
                ReadEvent _ _ revt ->
                    case resolvedEventDataAsJson revt of
                        Just js_evt ->
                            assertEqual "event should match" js js_evt
                        Nothing -> fail "Error when retrieving recorded data"
                _ -> fail "Event not found"
        e -> fail $ "Read failure: " <> show e

--------------------------------------------------------------------------------
deleteStreamTest :: Connection -> IO ()
deleteStreamTest conn = do
    stream <- freshStreamId
    let js  = object [ "baz" .= True ]
        evt = createEvent "foo" Nothing $ withJson js
    _ <- sendEvent conn stream anyVersion evt Nothing >>= wait
    _ <- deleteStream conn stream anyVersion Nothing Nothing
    return ()

--------------------------------------------------------------------------------
transactionTest :: Connection -> IO ()
transactionTest conn = do
    stream <- freshStreamId
    let js  = object [ "baz" .= True ]
        evt = createEvent "foo" Nothing $ withJson js
    t  <- startTransaction conn stream anyVersion Nothing >>= wait
    _  <- transactionWrite t [evt] Nothing >>= wait
    rs <- readEvent conn stream streamStart NoResolveLink Nothing >>= wait
    case rs of
        ReadNoStream -> return ()
        e -> fail $ "transaction-test stream is supposed to not exist "
                  <> show e
    _   <- transactionCommit t Nothing >>= wait
    rs2 <- readEvent conn stream streamStart NoResolveLink Nothing >>= wait
    case rs2 of
        ReadSuccess re ->
            case re of
                ReadEvent _ _ revt ->
                    case resolvedEventDataAsJson revt of
                        Just js_evt ->
                            assertEqual "event should match" js js_evt
                        Nothing -> fail "Error when retrieving recorded data"
                _ -> fail "Event not found"
        e -> fail $ "Read failure: " <> show e

--------------------------------------------------------------------------------
readStreamEventForwardTest :: Connection -> IO ()
readStreamEventForwardTest conn = do
    stream <- freshStreamId
    let jss = [ object [ "baz" .= True]
              , object [ "foo" .= False]
              , object [ "bar" .= True]
              ]
        evts = fmap (createEvent "foo" Nothing . withJson) jss
    _  <- sendEvents conn stream anyVersion evts Nothing >>= wait
    rs <- readEventsForward conn stream streamStart 10 NoResolveLink Nothing >>= wait
    case rs of
        ReadSuccess sl -> do
            let jss_evts = catMaybes $ fmap resolvedEventDataAsJson
                                     $ sliceEvents sl
            assertEqual "Events should be equal" jss jss_evts
        e -> fail $ "Read failure: " <> show e

--------------------------------------------------------------------------------
readStreamEventBackwardTest :: Connection -> IO ()
readStreamEventBackwardTest conn = do
    let jss = [ object [ "baz" .= True]
              , object [ "foo" .= False]
              , object [ "bar" .= True]
              ]
        evts = fmap (createEvent "foo" Nothing . withJson) jss
    _  <- sendEvents conn (StreamName "read-backward-test") anyVersion evts Nothing >>= wait
    let startFrom = eventNumber 2
    rs <- readEventsBackward conn (StreamName "read-backward-test") startFrom 10 NoResolveLink Nothing >>= wait
    case rs of
        ReadSuccess sl -> do
            let jss_evts = catMaybes $ fmap resolvedEventDataAsJson
                                     $ sliceEvents sl
            assertEqual "Events should be equal" (reverse jss) jss_evts
        e -> fail $ "Read failure: " <> show e

--------------------------------------------------------------------------------
readAllEventsForwardTest :: Connection -> IO ()
readAllEventsForwardTest conn = do
    sl <- readEventsForward conn All positionStart 3 NoResolveLink Nothing >>= wait
    assertEqual "Events is not empty" False (null $ sliceEvents sl)

--------------------------------------------------------------------------------
readAllEventsBackwardTest :: Connection -> IO ()
readAllEventsBackwardTest conn = do
    sl <- readEventsBackward conn All positionEnd 3 NoResolveLink Nothing >>= wait
    assertEqual "Events is not empty" False (null $ sliceEvents sl)

--------------------------------------------------------------------------------
subscribeTest :: Connection -> IO ()
subscribeTest conn = do
    stream <- freshStreamId

    let jss = [ object [ "baz" .= True]
              , object [ "foo" .= False]
              , object [ "bar" .= True]
              ]
        evts = fmap (createEvent "foo" Nothing . withJson) jss
    sub  <- subscribe conn stream NoResolveLink Nothing
    confirmChan <- newChan
    resultChan <- newChan


    fork $
      do let loop state@(acc, cnt)
               | cnt == 3
               = writeChan resultChan (reverse acc)

               | otherwise
               = do tpe <- nextSubEvent sub
                    case tpe of
                      Confirmed _
                        -> do writeChan confirmChan ()
                              loop state

                      Submit evt
                        -> loop (evt:acc, cnt+1)

                      Dropped _
                        -> writeChan resultChan (reverse acc)
         loop ([],0)

    readChan confirmChan
    _ <- sendEvents conn stream anyVersion evts Nothing >>= wait
    result <- fmap (fromJust . resolvedEventDataAsJson) <$> readChan resultChan

    assertEqual "Events should match" jss result

--------------------------------------------------------------------------------
subscribeFromTest :: Connection -> IO ()
subscribeFromTest conn = do
    stream <- freshStreamId

    let jss = [ object [ "1" .= (1 :: Int)]
              , object [ "2" .= (2 :: Int)]
              , object [ "3" .= (3 :: Int)]
              ]
        jss2 = [ object [ "4" .= (4 :: Int)]
               , object [ "5" .= (5 :: Int)]
               , object [ "6" .= (6 :: Int)]
               ]
        alljss = jss <> jss2
        evts   = fmap (createEvent "foo" Nothing . withJson) jss
        evts2  = fmap (createEvent "foo" Nothing . withJson) jss2
    _   <- sendEvents conn stream anyVersion evts Nothing >>= wait
    sub <- subscribeFrom conn stream NoResolveLink Nothing (Just 1) Nothing
    confirmChan <- newChan
    resultChan <- newChan


    fork $
      do let loop state@(acc, cnt)
               | cnt == 6
               = writeChan resultChan (reverse acc)

               | otherwise
               = do tpe <- nextSubEvent sub
                    case tpe of
                      Confirmed _
                        -> do writeChan confirmChan ()
                              loop state

                      Submit evt
                        -> do loop (evt:acc, cnt+1)

                      Dropped _
                        -> writeChan resultChan (reverse acc)
         loop ([],0)

    readChan confirmChan
    _ <- sendEvents conn stream anyVersion evts2 Nothing >>= wait
    result <- fmap (fromJust . resolvedEventDataAsJson) <$> readChan resultChan

    assertEqual "Events should match" alljss result

--------------------------------------------------------------------------------
subscribeFromNoStreamTest :: Connection -> IO ()
subscribeFromNoStreamTest conn = do
  stream <- freshStreamId
  sub <- subscribeFrom conn stream NoResolveLink Nothing Nothing Nothing
  confirmChan <- newChan
  resultChan <- newChan


  fork $
    do let loop state@(acc, cnt)
             | cnt == 3
             = writeChan resultChan (reverse acc)

             | otherwise
             = do tpe <- nextSubEvent sub
                  case tpe of
                    Confirmed _
                      -> do writeChan confirmChan ()
                            loop state

                    Submit evt
                      -> loop (evt:acc, cnt+1)

                    Dropped _
                      -> writeChan resultChan (reverse acc)
       loop ([],0)

  readChan confirmChan
  let jss = [ object [ "1" .= (1 :: Int)]
            , object [ "2" .= (2 :: Int)]
            , object [ "3" .= (3 :: Int)]
            ]

      evts = fmap (createEvent "foo" Nothing . withJson) jss

  _ <- sendEvents conn stream anyVersion evts Nothing >>= wait
  result <- fmap (fromJust . resolvedEventDataAsJson) <$> readChan resultChan

  assertEqual "Events should match" jss result

--------------------------------------------------------------------------------
setStreamMetadataTest :: Connection -> IO ()
setStreamMetadataTest conn = do
    stream <- freshStreamId
    let metadata = buildStreamMetadata $ setCustomProperty "foo" (1 :: Int)
    _ <- setStreamMetadata conn stream anyVersion metadata Nothing >>= wait
    return ()

--------------------------------------------------------------------------------
getStreamMetadataTest :: Connection -> IO ()
getStreamMetadataTest conn = do
    stream <- freshStreamId
    let metadata = buildStreamMetadata $ setCustomProperty "foo" (1 :: Int)
    _ <- setStreamMetadata conn stream anyVersion metadata Nothing >>= wait
    r <- getStreamMetadata conn stream Nothing >>= wait
    case r of
        StreamMetadataResult _ _ m ->
            case getCustomProperty m "foo" of
                Just i -> assertEqual "Should have equal value" (1 :: Int) i
                _      -> fail "Can't find foo property"
        _ -> fail $ "Stream " <> show stream <> " doesn't exist"

--------------------------------------------------------------------------------
emptyAclBytes :: ByteString
emptyAclBytes = $(embedFile "tests/fixtures/empty_acl.json")

--------------------------------------------------------------------------------
emptyMetaBytes :: ByteString
emptyMetaBytes = $(embedFile "tests/fixtures/empty_meta.json")

--------------------------------------------------------------------------------
aclSingleReadBytes :: ByteString
aclSingleReadBytes = $(embedFile "tests/fixtures/acl_single_read.json")

--------------------------------------------------------------------------------
aclMultipleReadBytes :: ByteString
aclMultipleReadBytes = $(embedFile "tests/fixtures/acl_multiple_read.json")

--------------------------------------------------------------------------------
metaPropBytes :: ByteString
metaPropBytes = $(embedFile "tests/fixtures/meta_prop.json")

--------------------------------------------------------------------------------
emptyAclParsing :: a -> IO ()
emptyAclParsing _ =
    assertEqual "should parse empty ACL"
        (Just emptyStreamACL)
        (decode $ fromStrict emptyAclBytes)

--------------------------------------------------------------------------------
aclSingleReadParsing :: a -> IO ()
aclSingleReadParsing _ =
    assertEqual "should parse single ACL property"
        (Just $ buildStreamACL $ setReadRole "iron")
        (decode $ fromStrict aclSingleReadBytes)

--------------------------------------------------------------------------------
aclMultipleReadParsing :: a -> IO ()
aclMultipleReadParsing _ =
    assertEqual "should parse multiple ACL property"
        (Just $ buildStreamACL $ setReadRoles ["iron", "game"])
        (decode $ fromStrict aclMultipleReadBytes)

--------------------------------------------------------------------------------
emptyStreamMetaParsing :: a -> IO ()
emptyStreamMetaParsing _ =
    assertEqual "should parse empty stream metadata"
        (Just emptyStreamMetadata)
        (decode $ fromStrict emptyMetaBytes)

--------------------------------------------------------------------------------
metaPropParsing :: a -> IO ()
metaPropParsing _ =
    assertEqual "should parse stream metadata property"
        (Just $ buildStreamMetadata $ setMaxCount 777)
        (decode $ fromStrict metaPropBytes)

--------------------------------------------------------------------------------
createPersistentTest :: Connection -> IO ()
createPersistentTest conn = do
    let def = defaultPersistentSubscriptionSettings
    stream <- freshStreamId
    r <- createPersistentSubscription conn "group" stream def Nothing >>= wait
    case r of
        Nothing -> return ()
        Just e  -> fail $ "Exception arised: " <> show e

--------------------------------------------------------------------------------
updatePersistentTest :: Connection -> IO ()
updatePersistentTest conn = do
    let def = defaultPersistentSubscriptionSettings
    stream <- freshStreamId
    _ <- createPersistentSubscription conn "group" stream def Nothing >>= wait
    r <- updatePersistentSubscription conn "group" stream def Nothing >>= wait
    case r of
        Nothing -> return ()
        Just e  -> fail $ "Exception arised: " <> show e

--------------------------------------------------------------------------------
deletePersistentTest :: Connection -> IO ()
deletePersistentTest conn = do
    let def = defaultPersistentSubscriptionSettings
    stream <- freshStreamId
    _ <- createPersistentSubscription conn "group" stream def Nothing >>= wait
    r <- deletePersistentSubscription conn "group" stream Nothing >>= wait
    case r of
        Nothing -> return ()
        Just e  -> fail $ "Exception arised: " <> show e

--------------------------------------------------------------------------------
connectToPersistentTest :: Connection -> IO ()
connectToPersistentTest conn = do
    let def = defaultPersistentSubscriptionSettings
        js1 = object ["baz" .= True]
        js2 = object ["foo" .= True]
        jss  = [ js1
               , js2
               ]
        evts = fmap (createEvent "foo" Nothing . withJson) jss
    stream <- freshStreamId
    _   <- createPersistentSubscription conn "group" stream def Nothing >>= wait
    sub <- connectToPersistentSubscription conn "group" stream 1 Nothing
    confirmChan <- newChan
    resultChan <- newChan

    fork $
      do let loop state@(acc, cnt)
               | cnt == 2
               = writeChan resultChan (reverse acc)

               | otherwise
               = do tpe <- nextSubEvent sub
                    case tpe of
                      Confirmed _
                        -> do writeChan confirmChan ()
                              loop state

                      Submit evt
                        -> do acknowledge sub evt
                              loop (evt:acc, cnt+1)

                      Dropped _
                        -> writeChan resultChan (reverse acc)
         loop ([],0)

    readChan confirmChan
    _ <- sendEvents conn stream anyVersion evts Nothing >>= wait
    result <- fmap (fromJust . resolvedEventDataAsJson) <$> readChan resultChan

    assertEqual "Both events should match" [js1,js2] result

--------------------------------------------------------------------------------
maxAgeTest :: Connection -> IO ()
maxAgeTest conn = do
    let timespan = fromDays 1
        metadata = buildStreamMetadata $ setMaxAge timespan
        evt = createEvent "foo" Nothing
              $ withJson (object ["type" .= (3 :: Int)])
    stream <- freshStreamId
    _ <- sendEvent conn stream anyVersion evt Nothing >>= wait
    _ <- setStreamMetadata conn stream anyVersion metadata Nothing >>= wait
    r <- getStreamMetadata conn stream Nothing >>= wait
    case r of
        StreamMetadataResult _ _ m ->
            assertEqual "Should have equal timespan" (Just timespan)
            (streamMetadataMaxAge m)
        _ -> fail $ "Stream " <> show stream <> " doesn't exist"

--------------------------------------------------------------------------------
generateEvents :: Int -> [Value]
generateEvents n = take n $ fmap toObj [1..]

--------------------------------------------------------------------------------
toObj :: Int -> Value
toObj n = object [ pack (show n) .= n ]

--------------------------------------------------------------------------------
streamRegularStreamForwardTest :: Connection -> IO ()
streamRegularStreamForwardTest conn = do
    stream <- freshStreamId

    let jss  = generateEvents 10
        evts = fmap (createEvent "foo" Nothing . withJson) jss
        src = throwOnError $ readThroughForward conn stream NoResolveLink streamStart (Just 1) Nothing

    _ <- sendEvents conn stream anyVersion evts Nothing >>= wait
    rest <- Streaming.foldM_ check (pure [1..10]) pure src
    assertEqual "Should be empty" [] rest
  where
    check (x:xs) e =
        case resolvedEventDataAsJson e of
            Just e | e == toObj x -> pure xs
                   | otherwise    -> fail "Out of order event's appeared (stream)"
            _ -> fail "Can't deserialized event"

--------------------------------------------------------------------------------
streamRegularStreamBackwardTest :: Connection -> IO ()
streamRegularStreamBackwardTest conn = do
    stream <- freshStreamId

    let jss  = generateEvents 10
        evts = fmap (createEvent "foo" Nothing . withJson) jss
        src = throwOnError $ readThroughBackward conn stream NoResolveLink streamEnd (Just 1) Nothing

    _ <- sendEvents conn stream anyVersion evts Nothing >>= wait
    rest <- Streaming.foldM_ check (pure $ reverse [1..10]) pure src
    assertEqual "Should be empty" [] rest
  where
    check (x:xs) e =
        case resolvedEventDataAsJson e of
            Just e | e == toObj x -> pure xs
                   | otherwise    -> fail "Out of order event's appeared (stream)"
            _ -> fail "Can't deserialized event"
