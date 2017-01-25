{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}
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
-- Top level operation and subscription logic of EventStore driver.
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Processor
    ( Proc
    , ProcOutcome(..)
    , newProc
    , executeCmd
    , executeAbort
    , executePkg
    ) where

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Cmd
import Database.EventStore.Internal.EndPoint
import Database.EventStore.Internal.Generator
import Database.EventStore.Internal.Publish
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
import qualified Database.EventStore.Internal.Manager.Operation.Model as Op
import qualified Database.EventStore.Internal.Manager.Subscription.Driver as Sub

--------------------------------------------------------------------------------
data ProcOutcome
  = ProcSendPkg !Package
  | ProcExecJob !(IO ())
  | ProcReconnect !NodeEndPoints
  | ProcNoop

--------------------------------------------------------------------------------
data Proc =
  Proc { _setts :: Settings
       , _pub :: Publish ProcOutcome
       , _opRef :: IORef (Op.Model (IO ()))
       , _subRef :: IORef (Sub.Driver (IO ()))
       }

--------------------------------------------------------------------------------
newProc :: Settings -> Publish ProcOutcome -> IO Proc
newProc setts pub = do
  gen <- newGenerator
  let (g1, g2) = splitGenerator gen

  Proc setts pub <$> newIORef (Op.newModel setts g1)
                 <*> newIORef (Sub.newDriver setts g2)

--------------------------------------------------------------------------------
withSubDriver :: Proc
              -> (Sub.Driver (IO ()) -> (Sub.Driver (IO ()), Package))
              -> IO ()
withSubDriver Proc{..} k =
    publish _pub . ProcSendPkg =<< atomicModifyIORef' _subRef k

--------------------------------------------------------------------------------
loopOp :: Op.Transition (IO ()) -> (Op.Model (IO ()), [ProcOutcome])
loopOp = go []
  where
    go ps (Op.Produce p nxt) =
        go (ProcExecJob p:ps) nxt
    go ps (Op.Transmit pkg nxt) =
        go (ProcSendPkg pkg:ps) nxt
    go ps (Op.NotHandled info nxt) =
        let node = masterInfoNodeEndPoints info in
        go (ProcReconnect node:ps) nxt
    go ps (Op.Await model) = (model, ps)

--------------------------------------------------------------------------------
withOpModel :: Proc
            -> (Op.Model (IO ()) -> Op.Transition (IO ()))
            -> IO ()
withOpModel Proc{..} k = do
    orders <- atomicModifyIORef' _opRef (loopOp . k)
    traverse_ (publish _pub) orders

--------------------------------------------------------------------------------
executeCmd :: Proc -> Cmd -> IO ()
executeCmd p cmd =
    case cmd of
        NewOperation k op ->
          withOpModel p (Op.pushOperation k op)
        SubCmd subCmd ->
          executeSubCmd p subCmd

--------------------------------------------------------------------------------
noop :: IO ()
noop = return ()

--------------------------------------------------------------------------------
executeSubCmd :: Proc -> SubCmd -> IO ()
executeSubCmd p c = withSubDriver p (swap . action c)
  where
    action (ConnectStream k s tos) = Sub.connectToStream k s tos
    action (ConnectPersist k g s b) = Sub.connectToPersist k g s b
    action (Unsubscribe r) = Sub.unsubscribe r
    action (CreatePersist k g s ss) = Sub.createPersist k g s ss
    action (UpdatePersist k g s ss) = Sub.updatePersist k g s ss
    action (DeletePersist k g s) = Sub.deletePersist k g s
    action (AckPersist run evts) = Sub.ackPersist noop run evts
    action (NakPersist run act res evts) = Sub.nakPersist noop run act res evts

--------------------------------------------------------------------------------
-- | Aborts every pending operation.
executeAbort :: Proc -> IO ()
executeAbort p@Proc{..} = do
  withOpModel p Op.abort

  drv <- readIORef _subRef
  traverse_ (publish _pub . ProcExecJob) $ Sub.abort drv

--------------------------------------------------------------------------------
executePkg :: Proc -> Package -> IO ()
executePkg p@Proc{..} pkg@Package{..}
  | packageCmd == 0x01 = do
        let resp = heartbeatResponsePackage packageCorrelation
        publish _pub (ProcSendPkg resp)
  | otherwise = do
        m <- readIORef _opRef
        case Op.submitPackage pkg m of
            Just sm -> withOpModel p $ \_ -> sm
            Nothing -> do
                drv <- readIORef _subRef
                for_ (Sub.submitPackage pkg drv) $ \(job, drv') -> do
                    atomicWriteIORef _subRef drv'
                    publish _pub (ProcExecJob job)
