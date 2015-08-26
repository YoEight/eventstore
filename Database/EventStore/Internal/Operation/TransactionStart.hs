{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Operation.TransactionStart
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Operation.TransactionStart where

--------------------------------------------------------------------------------
import Data.Int

--------------------------------------------------------------------------------
import Data.Text

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Operation
import Database.EventStore.Internal.Operation.Write.Common
import Database.EventStore.Internal.Types

--------------------------------------------------------------------------------
data Transaction =
    Transaction
    { _tId              :: !Int64
    , _tStream          :: !Text
    , _tExpectedVersion :: !ExpectedVersion
    , _tWriteEventsOp   :: [Event] -> Operation 'Init ()
    , _tCommitOp        :: Operation 'Init WriteResult
    }