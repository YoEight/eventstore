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
data Elem r =
    forall a.
    Elem
    { _eAttempt :: !Int
    , _eOp      :: !(Operation 'Pending a)
    , _cb       :: Either OperationError a -> r
    }

--------------------------------------------------------------------------------
data State r =
    State
    { _gen     :: Generator
    , _pending :: H.HashMap UUID (Elem r)
    }

--------------------------------------------------------------------------------
initState :: Generator -> State r
initState g = State g H.empty

--------------------------------------------------------------------------------
data In r
    = forall a. New (Operation 'Init a) (Either OperationError a -> r)
    | Pkg Package

--------------------------------------------------------------------------------
data Step r
    = Done r (Model r)
    | Send Package (Model r)
    | Cont (Model r)

--------------------------------------------------------------------------------
newtype Model r = Model (In r -> Maybe (Step r))

--------------------------------------------------------------------------------
pushOperation :: (Either OperationError a -> r)
              -> Operation 'Init a
              -> Model r
              -> (Package, Model r)
pushOperation cb op (Model k) =
    let Just (Send pkg nxt) = k (New op cb) in (pkg, nxt)

--------------------------------------------------------------------------------
submitPackage :: Package -> Model r -> Maybe (Step r)
submitPackage pkg (Model k) = k (Pkg pkg)

--------------------------------------------------------------------------------
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
