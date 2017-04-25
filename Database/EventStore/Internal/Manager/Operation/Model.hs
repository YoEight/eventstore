{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Manager.Operation.Model
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
-- Main operation bookkeeping structure.
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Manager.Operation.Model
    ( Model
    , Transition(..)
    , newModel
    , pushOperation
    , submitPackage
    , abort
    ) where

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.ProtocolBuffers
import Data.Serialize
import Data.UUID
import Data.UUID.V4

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Callback
import Database.EventStore.Internal.Command
import Database.EventStore.Internal.Communication
import Database.EventStore.Internal.Generator
import Database.EventStore.Internal.Messaging
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Entry of a running 'Operation'.
data Elem r =
    forall a resp. Decode resp =>
    Elem
    { _opOp      :: !(Operation a)
    , _opCmd     :: !Command
    , _opRetries :: !Int
    , _opCont    :: resp -> SM a ()
    , _opCb      :: Either OperationError a -> r
    }

--------------------------------------------------------------------------------
data Pending =
    forall result response. Decode response =>
    Pending { _pendingOp       :: !(Operation result)
            , _pendingRespCmd  :: !Command
            , _pendingRetries  :: !Int
            , _pendingResume   :: response -> SM result ()
            , _pendingCallback :: !(Callback result)
            }

--------------------------------------------------------------------------------
data Registry =
    Registry { _regSettings :: Settings
              , _regBus      :: Bus
              , _regPendings :: IORef (HashMap UUID Pending)
              }

--------------------------------------------------------------------------------
newRegistry :: Settings -> Bus -> IO Registry
newRegistry setts bus = Registry setts bus <$> newIORef mempty

--------------------------------------------------------------------------------
register :: Registry -> Operation a -> Callback a -> IO ()
register reg op cb = evaluate reg op cb op

--------------------------------------------------------------------------------
abortPendingRequests :: Registry -> IO ()
abortPendingRequests Registry{..} = do
  m <- readIORef _regPendings
  atomicWriteIORef _regPendings mempty

  for_ m $ \Pending{..} -> reject _pendingCallback Aborted

--------------------------------------------------------------------------------
handlePackage :: Registry -> Package -> IO ()
handlePackage reg@Registry{..} pkg@Package{..} = do
  m <- readIORef _regPendings

  atomicModifyIORef' _regPendings $ \m ->
    (deleteMap packageCorrelation m, ())

  for_ (lookup packageCorrelation m) $ \Pending{..} ->
    case packageCmd of
      cmd
        | cmd == badRequestCmd -> do
          let reason = packageDataAsText pkg

          reject _pendingCallback (ServerError reason)
        | cmd == notAuthenticatedCmd ->
          reject _pendingCallback NotAuthenticatedOp
        | cmd == notHandledCmd -> do
          let Just msg = maybeDecodeMessage packageData
              reason   = getField $ notHandledReason msg
          case reason of
            N_NotMaster -> do
              let Just details = getField $ notHandledAdditionalInfo msg
                  info         = masterInfo details
                  node         = masterInfoNodeEndPoints info

              publish _regBus (ForceReconnect node)
              evaluate reg _pendingOp _pendingCallback _pendingOp
            -- In this case with just retry the operation.
            _ -> evaluate reg _pendingOp _pendingCallback _pendingOp
        | cmd /= _pendingRespCmd ->
          reject _pendingCallback (InvalidServerResponse _pendingRespCmd cmd)
        | otherwise ->
          case runGet decodeMessage packageData of
            Left e ->
              reject _pendingCallback (ProtobufDecodingError e)
            Right resp ->
              evaluate reg _pendingOp _pendingCallback (_pendingResume resp)

--------------------------------------------------------------------------------
evaluate :: Registry
         -> Operation a
         -> Callback a
         -> SM a ()
         -> IO ()
evaluate Registry{..} op cb = go
  where
    go (Return _)  = return ()
    go (Yield a next) = do
      fulfill cb a
      go next
    go (FreshId k) = do
      uuid <- nextRandom
      go (k uuid)
    go (SendPkg cmdReq cmdResp req next) = do
      uuid <- nextRandom
      let pkg = Package { packageCmd         = cmdReq
                        , packageCorrelation = uuid
                        , packageData        = runPut $ encodeMessage req
                        , packageCred        = s_credentials _regSettings
                        }

          pending = Pending { _pendingOp       = op
                            , _pendingRespCmd  = cmdResp
                            , _pendingRetries  = 1
                            , _pendingResume   = next
                            , _pendingCallback = cb
                            }
      atomicModifyIORef' _regPendings $ \m ->
        (insertMap uuid pending m, ())

      publish _regBus (TcpSend pkg)
    go (Failure outcome) =
      case outcome of
        Just e  -> reject cb e
        Nothing -> go op

--------------------------------------------------------------------------------
-- | Operation internal state.
data State r =
    State
    { _setts :: Settings
    , _gen :: Generator
      -- ^ 'UUID' generator.
    , _pending :: HashMap UUID (Elem r)
      -- ^ Contains all running 'Operation's.
    }

--------------------------------------------------------------------------------
initState :: Settings -> Generator -> State r
initState setts g = State setts g mempty

--------------------------------------------------------------------------------
-- | Type of requests handled by the model.
data Request r
    = forall a. New (Operation a) (Either OperationError a -> r)
      -- ^ Register a new 'Operation'.
    | Pkg Package
      -- ^ Submit a package.
    | Abort
      -- ^ Aborts every pending operation.

--------------------------------------------------------------------------------
-- | Output produces by the interpretation of an 'Operation'.
data Transition r
    = Produce r (Transition r)
      -- ^ Produces an intermediary value.
    | Transmit Package (Transition r)
      -- ^ Asks for sending the given 'Package'.
    | Await (Model r)
      -- ^ waits for more input.
    | NotHandled MasterInfo (Transition r)

--------------------------------------------------------------------------------
-- | Main 'Operation' bookkeeping state machine.
newtype Model r = Model (Request r -> Maybe (Transition r))

--------------------------------------------------------------------------------
-- | Pushes a new 'Operation' to model. The given 'Operation' state-machine is
--   initialized and produces a 'Package'.
pushOperation :: (Either OperationError a -> r)
              -> Operation a
              -> Model r
              -> Transition r
pushOperation cb op (Model k) = let Just t = k (New op cb) in t

--------------------------------------------------------------------------------
-- | Submits a 'Package' to the model. If the model isn't concerned by the
--   'Package', it will returns 'Nothing'. Because 'Operation' can implement
--   complex logic (retry for instance), it returns a 'Step'.
submitPackage :: Package -> Model r -> Maybe (Transition r)
submitPackage pkg (Model k) = k (Pkg pkg)

--------------------------------------------------------------------------------
-- | Aborts every pending operation.
abort :: Model r -> Transition r
abort (Model k) = let Just t = k Abort in t

--------------------------------------------------------------------------------
runOperation :: Settings
             -> (Either OperationError a -> r)
             -> Operation a
             -> SM a ()
             -> State r
             -> Transition r
runOperation setts cb op start init_st = go init_st start
  where
    go st (Return _) = Await $ Model $ execute setts st
    go st (Yield a n) = Produce (cb $ Right a) (go st n)
    go st (FreshId k) =
        let (new_id, nxt_gen) = nextUUID $ _gen st
            nxt_st            = st { _gen = nxt_gen } in
        go nxt_st $ k new_id
    go st (SendPkg ci co rq k) =
        let (new_uuid, nxt_gen) = nextUUID $ _gen st
            pkg = Package
                  { packageCmd         = ci
                  , packageCorrelation = new_uuid
                  , packageData        = runPut $ encodeMessage rq
                  , packageCred        = s_credentials setts
                  }
            elm    = Elem op co 1 k cb
            ps     = insertMap new_uuid elm $ _pending st
            nxt_st = st { _pending = ps
                        , _gen     = nxt_gen
                        } in
        Transmit pkg (Await $ Model $ execute setts nxt_st)
    go st (Failure m) =
        case m of
            Just e -> Produce (cb $ Left e) (Await $ Model $ execute setts st)
            _      -> runOperation setts cb op op st

--------------------------------------------------------------------------------
runPackage :: Settings -> State r -> Package -> Maybe (Transition r)
runPackage setts st pkg@Package{..} = do
    Elem{..} <- lookup packageCorrelation $ _pending st
    let nxt_ps = deleteMap packageCorrelation $ _pending st
        nxt_st = st { _pending = nxt_ps }
    case packageCmd of
        cmd | cmd == badRequestCmd -> do
            let reason = packageDataAsText pkg
                resp  = ServerError reason
                value = _opCb $ Left $ resp
            return $ Produce value (Await $ Model $ execute setts nxt_st)
            | cmd == notAuthenticatedCmd -> do
            let value = _opCb $ Left NotAuthenticatedOp
            return $ Produce value (Await $ Model $ execute setts nxt_st)
            | cmd == notHandledCmd -> do
            msg <- maybeDecodeMessage packageData
            let reason = getField $ notHandledReason msg
            case reason of
                N_NotMaster -> do
                    info <- getField $ notHandledAdditionalInfo msg
                    let next = runOperation setts _opCb _opOp _opOp nxt_st
                    return $ NotHandled (masterInfo info) next
                -- In this case with just retry the operation.
                _ -> return $ runOperation setts _opCb _opOp _opOp nxt_st
        _ | packageCmd /= _opCmd -> do
              let r = _opCb $ Left $ InvalidServerResponse _opCmd packageCmd
              return $ Produce r (Await $ Model $ execute setts nxt_st)
          | otherwise ->
              case runGet decodeMessage packageData of
                  Left e  ->
                      let r = _opCb $ Left $ ProtobufDecodingError e in
                      return $ Produce r (Await $ Model $ execute setts nxt_st)
                  Right m -> return $
                      runOperation setts _opCb _opOp (_opCont m) nxt_st

--------------------------------------------------------------------------------
abortOperations :: Settings -> State r -> Transition r
abortOperations setts init_st = go init_st $ mapToList $ _pending init_st
  where
    go st ((key, Elem{..}):xs) =
        let ps     = deleteMap key $ _pending st
            nxt_st = st { _pending = ps } in
        Produce (_opCb $ Left Aborted) $ go nxt_st xs
    go st [] = Await $ Model $ execute setts st

--------------------------------------------------------------------------------
-- | Creates a new 'Operation' model state-machine.
newModel :: Settings -> Generator -> Model r
newModel setts g = Model $ execute setts $ initState setts g

--------------------------------------------------------------------------------
execute :: Settings -> State r -> Request r -> Maybe (Transition r)
execute setts st (New op cb) = Just $ runOperation setts cb op op st
execute setts st (Pkg pkg)   = runPackage setts st pkg
execute setts st Abort       = Just $ abortOperations setts st

--------------------------------------------------------------------------------
maybeDecodeMessage :: Decode a => ByteString -> Maybe a
maybeDecodeMessage bytes =
    case runGet decodeMessage bytes of
        Right a -> Just a
        _       -> Nothing
