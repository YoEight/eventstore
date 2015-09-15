{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures            #-}
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
    , Step(..)
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
    { _eAttempt :: !Int
      -- ^ How many time that operation failed.
    , _eOp :: !(Operation 'Pending a)
      -- ^ Current operation state-machine.
    , _cb :: Either OperationError a -> r
      -- ^ Callback called on operation completion.
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
    = forall a. New (Operation 'Init a) (Either OperationError a -> r)
      -- ^ Register a new 'Operation'.
    | Pkg Package
      -- ^ Submit a package.

--------------------------------------------------------------------------------
-- | Model logic when serving a 'Request'.
data Step r
    = Done r (Model r)
      -- ^ The model was able to produce a final value.
    | Send Package (Model r)
      -- ^ The model wants that 'Package' to be sent to the server.
    | Cont (Model r)
      -- ^ The model only update its internal state with no intermediary value.

--------------------------------------------------------------------------------
newtype Model r = Model (Request r -> Maybe (Step r))

--------------------------------------------------------------------------------
-- | Pushes a new 'Operation' to model. The given 'Operation' state-machine is
--   initialized and produces a 'Package'.
pushOperation :: (Either OperationError a -> r)
              -> Operation 'Init a
              -> Model r
              -> (Package, Model r)
pushOperation cb op (Model k) =
    let Just (Send pkg nxt) = k (New op cb) in (pkg, nxt)

--------------------------------------------------------------------------------
-- | Submits a 'Package' to the model. If the model isn't concerned by the
--   'Package', it will returns 'Nothing'. Because 'Operation' can implement
--   complex logic (retry for instance), it returns a 'Step'.
submitPackage :: Package -> Model r -> Maybe (Step r)
submitPackage pkg (Model k) = k (Pkg pkg)

--------------------------------------------------------------------------------
-- | Creates a new 'Operation' model state-machine.
newModel :: Generator -> Model r
newModel g = Model $ go $ initState g
  where
    go st i =
        case i of
            New op cb ->
                let (u, nxt_g) = nextUUID $ _gen st
                    (pkg, p)   = createPackage u op
                    elm        = Elem 0 p cb
                    ps         = H.insert u elm $ _pending st
                    nxt_st     = st { _gen     = nxt_g
                                    , _pending = ps
                                    } in
                Just $ Send pkg (Model $ go nxt_st)
            Pkg pkg ->
                case H.lookup (packageCorrelation pkg) $ _pending st of
                    Nothing              -> Nothing
                    Just (Elem at op cb) -> Just $
                        case packageArrived pkg op of
                            Left op' ->
                                let elm    = Elem at op' cb
                                    m      = _pending st
                                    u      = packageCorrelation pkg
                                    ps     = H.insert u elm m
                                    nxt_st = st { _pending = ps } in
                                Cont $ Model $ go nxt_st
                            Right c ->
                                case getReport c of
                                    Retry op' ->
                                        let (u, nxt_g) = nextUUID $ _gen st
                                            (rpkg, p)  = createPackage u op'
                                            elm        = Elem (at+1) p cb
                                            m          = _pending st
                                            ps         = H.insert u elm m
                                            nxt_st     = st { _pending = ps
                                                            , _gen     = nxt_g
                                                            } in
                                        Send rpkg (Model $ go nxt_st)
                                    Error e ->
                                        let r      = cb (Left e)
                                            u      = packageCorrelation pkg
                                            ps     = H.delete u $ _pending st
                                            nxt_st = st { _pending = ps } in
                                        Done r (Model $ go nxt_st)
                                    Success a ->
                                        let r      = cb (Right a)
                                            u      = packageCorrelation pkg
                                            ps     = H.delete u $ _pending st
                                            nxt_st = st { _pending = ps } in
                                        Done r (Model $ go nxt_st)
