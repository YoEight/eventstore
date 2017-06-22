--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Utils
-- Copyright : (C) 2016 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Utils (prettyWord8) where

--------------------------------------------------------------------------------
import Prelude (String)
import Numeric

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Prelude

--------------------------------------------------------------------------------
prettyWord8 :: Word8 -> String
prettyWord8 w = "0x" <> padding (showHex w "")

--------------------------------------------------------------------------------
padding :: String -> String
padding [x] = ['0',x]
padding xs  = xs
