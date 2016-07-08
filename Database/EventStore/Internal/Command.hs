{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Command
-- Copyright : (C) 2016 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Command (Command(..)) where

--------------------------------------------------------------------------------
import Data.Word
import Numeric

--------------------------------------------------------------------------------
-- | Internal command representation.
newtype Command = Command { cmdWord8 :: Word8 } deriving (Eq, Ord, Num)

--------------------------------------------------------------------------------
padding :: String -> String
padding [x] = ['0',x]
padding xs  = xs

--------------------------------------------------------------------------------
instance Show Command where
    show (Command w) = "0x" ++ padding (showHex w "")
