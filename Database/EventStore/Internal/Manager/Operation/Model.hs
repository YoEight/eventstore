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
    ) where

--------------------------------------------------------------------------------
import qualified Data.HashMap.Strict  as H
import           Data.UUID

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Generator
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
-- | Entry of a running 'Operation'.
data Elem r =
    forall a.
    Elem
    { _opId   :: UUID
    , _opOp   :: Operation a
    , _opCont :: Package -> SM a ()
    , _opCb   :: Either OperationError a -> r
    }

--------------------------------------------------------------------------------
-- | Operation internal state.
data State r =
    State
    { _gen :: Generator
      -- ^ 'UUID' generator.
    , _pending :: H.HashMap UUID (Elem r)
      -- ^ Contains all running 'Operation's.
    }

--------------------------------------------------------------------------------
initState :: Generator -> State r
initState g = State g H.empty

--------------------------------------------------------------------------------
-- | Type of requests handled by the model.
data Request r
    = forall a. New (Operation a) (Either OperationError a -> r)
      -- ^ Register a new 'Operation'.
    | Pkg Package
      -- ^ Submit a package.

--------------------------------------------------------------------------------
data Transition r
    = Produce r (Transition r)
    | Transmit Package (Transition r)
    | Await (Model r)

--------------------------------------------------------------------------------
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
runOperation :: (Either OperationError a -> r)
             -> UUID
             -> SM a ()
             -> Operation a
             -> State r
             -> Transition r
runOperation cb op_id start op init_st = go init_st start
  where
    go st (Return _) =
        let nxt_ps = H.delete op_id $ _pending st
            nxt_st = st { _pending = nxt_ps } in
        Await $ Model $ handle nxt_st
    go st (Yield a n) = Produce (cb $ Right a) (go st n)
    go st (FreshId k) =
        let (new_id, nxt_gen) = nextUUID $ _gen st
            nxt_st            = st { _gen = nxt_gen } in
        go nxt_st $ k new_id
    go st (SendPkg pkg k) =
        let elm    = Elem op_id op k cb
            ps     = H.insert op_id elm $ _pending st
            nxt_st = st { _pending = ps } in
        Transmit pkg (Await $ Model $ handle nxt_st)
    go st (Failure m) =
        let nxt_ps = H.delete op_id $ _pending st
            nxt_st = st { _pending = nxt_ps } in
        case m of
            Just e -> Produce (cb $ Left e) (Await $ Model $ handle nxt_st)
            _      ->
                let (new_id, nxt_gen) = nextUUID $ _gen nxt_st
                    fin_st            = nxt_st { _gen = nxt_gen } in
                runOperation cb new_id (applyOp op new_id) op fin_st

--------------------------------------------------------------------------------
runPackage :: Package -> State r -> Maybe (Transition r)
runPackage pkg st = do
    Elem op_id op cont cb <- H.lookup (packageCorrelation pkg) $ _pending st
    return $ runOperation cb op_id (cont pkg) op st

--------------------------------------------------------------------------------
-- | Creates a new 'Operation' model state-machine.
newModel :: Generator -> Model r
newModel g = Model $ handle $ initState g

--------------------------------------------------------------------------------
handle :: State r -> Request r -> Maybe (Transition r)
handle st (New op cb) =
    let (op_id, nxt_gen) = nextUUID $ _gen st
        nxt_st           = st { _gen = nxt_gen } in
    Just $ runOperation cb op_id (applyOp op op_id) op nxt_st
handle st (Pkg pkg) = runPackage pkg st
