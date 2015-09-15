{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Processor
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Processor
    ( Processor
    , newProcessor
    ) where

--------------------------------------------------------------------------------
import Data.Int

--------------------------------------------------------------------------------
import Data.Text

--------------------------------------------------------------------------------
import           Database.EventStore.Internal.Generator
import qualified Database.EventStore.Internal.Manager.Subscription as Sub
import           Database.EventStore.Internal.Operation
import qualified Database.EventStore.Internal.Manager.Operation.Model as Op
import           Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Instr r
    = Cmd (Cmd r)
    | Pkg Package

--------------------------------------------------------------------------------
data Cmd r
    = SubscriptionCmd SubscriptionCmd
    | forall a. NewOp (Operation 'Init a) (Either OperationError a -> r)

--------------------------------------------------------------------------------
data SubscriptionCmd
    = ConnectStream Text Bool
    | ConnectPersist Text Text Int32
    | CreatePersist Text Text PersistentSubscriptionSettings
    | UpdatePersist Text Text PersistentSubscriptionSettings
    | DeletePersist Text Text

--------------------------------------------------------------------------------
data Step r
    = Cont (Processor r)
    | Done r (Processor r)
    | Send Package (Processor r)

--------------------------------------------------------------------------------
data State r =
    State
    { _subDriver :: Sub.Driver
    , _opModel   :: Op.Model r
    }

--------------------------------------------------------------------------------
initState :: Settings -> Generator -> State r
initState setts g = State (Sub.newDriver setts g1) (Op.newModel g2)
  where
    (g1, g2) = splitGenerator g

--------------------------------------------------------------------------------
newtype Processor r = Processor (Instr r -> Step r)

--------------------------------------------------------------------------------
newProcessor :: Settings -> Generator -> Processor r
newProcessor setts gen = Processor $ go $ initState setts gen
  where
    go st (Cmd tpe) =
        case tpe of
            NewOp op cb ->
                let (pkg, nxt_m) = Op.pushOperation cb op $ _opModel st
                    nxt_st       = st { _opModel = nxt_m } in
                Send pkg (Processor $ go nxt_st)
            SubscriptionCmd cmd -> subCmd st cmd
    go st (Pkg pkg) =
        case Op.submitPackage pkg $ _opModel st of
            Just (Op.Done r nxt_m) ->
                let nxt_st = st { _opModel = nxt_m } in
                Done r (Processor $ go nxt_st)
            Just (Op.Send npkg nxt_m) ->
                let nxt_st = st { _opModel = nxt_m } in
                Send npkg (Processor $ go nxt_st)
            Just (Op.Cont nxt_m) ->
                let nxt_st = st { _opModel = nxt_m } in
                Cont $ Processor $ go nxt_st
            Nothing ->
                case Sub.submitPackage pkg $ _subDriver st of
                    Nothing             -> Cont $ Processor $ go st
                    Just (sub, nxt_drv) -> undefined

    subCmd st@State{..} cmd =
        case cmd of
            ConnectStream s tos ->
                let (pkg, nxt_drv) = Sub.connectToStream s tos _subDriver
                    nxt_st         = st { _subDriver = nxt_drv }
                    nxt            = Processor $ go nxt_st in
                Send pkg nxt
            ConnectPersist g s b ->
                let (pkg, nxt_drv) = Sub.connectToPersist g s b _subDriver
                    nxt_st         = st { _subDriver = nxt_drv }
                    nxt            = Processor $ go nxt_st in
                Send pkg nxt
            CreatePersist g s ss ->
                let (pkg, nxt_drv) = Sub.createPersist g s ss _subDriver
                    nxt_st         = st { _subDriver = nxt_drv }
                    nxt            = Processor $ go nxt_st in
                Send pkg nxt
            UpdatePersist g s ss ->
                let (pkg, nxt_drv) = Sub.updatePersist g s ss _subDriver
                    nxt_st         = st { _subDriver = nxt_drv }
                    nxt            = Processor $ go nxt_st in
                Send pkg nxt
            DeletePersist g s ->
                let (pkg, nxt_drv) = Sub.deletePersist g s _subDriver
                    nxt_st         = st { _subDriver = nxt_drv }
                    nxt            = Processor $ go nxt_st in
                Send pkg nxt
