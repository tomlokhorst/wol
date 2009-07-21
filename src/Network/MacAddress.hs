-------------------------------------------------------------------------------
-- |
-- Module      : Network.MacAddress
-- Copyright   : (c) 2009, Tom Lokhorst
-- License     : BSD3
--
-- Maintainer  : Tom Lokhorst <tom@lokhorst.eu>
-- Stability   : Experimental
--
-- A 'sort of' reusable module for mac addresses.
--
-------------------------------------------------------------------------------
module Network.MacAddress
  ( MacAddress (..)
  , parse
  , bytes
  ) where

import Data.List.Split
import Data.Word
import Numeric

data MacAddress = MacAddress Word8 Word8 Word8 Word8 Word8 Word8

instance Show MacAddress where
  show (MacAddress b0 b1 b2 b3 b4 b5) = 
    ( showHex b0 . colon . showHex b1 . colon . showHex b2 . colon
    . showHex b3 . colon . showHex b4 . colon . showHex b5) ""
    where
      colon = showString ":"

-- | Partial function to "parse" a `String` to a `MacAddress`
parse :: String -> MacAddress
parse s =
    let parts = splitOn ":" s
        words = map (fst . head . readHex) parts
    in if length words /= 6
       then error $ "Network.MacAddress.parse: malformed MacAddress '" ++ s ++ "'"
       else MacAddress (words !! 0) (words !! 1) (words !! 2)
                       (words !! 3) (words !! 4) (words !! 5)

-- | Get all bytes in a mac address
bytes :: MacAddress -> [Word8]
bytes (MacAddress b0 b1 b2 b3 b4 b5) = [b0, b1, b2, b3, b4, b5]

