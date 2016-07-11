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
import Control.Monad

data MacAddress = MacAddress Word8 Word8 Word8 Word8 Word8 Word8

instance Show MacAddress where
  show (MacAddress b0 b1 b2 b3 b4 b5) = 
    ( showHex b0 . colon . showHex b1 . colon . showHex b2 . colon
    . showHex b3 . colon . showHex b4 . colon . showHex b5) ""
    where
      colon = showString ":"

-- | Partial function to "parse" a `String` to a `MacAddress`
parse :: String -> Either String MacAddress
parse s = if length parts /= 6
          then Left "length of parts /= 6"
          else foldM func [] parts >>= \w ->
               return $ MacAddress (w !! 0) (w !! 1) (w !! 2)
                                   (w !! 3) (w !! 4) (w !! 5)
  where
    parts = splitOn ":" s
    func h l = case readHex l of
                 []      -> Left $ l ++ " is not a hex number"
                 [(a,b)] -> if null b && a <= 0xff
                            then return $ h ++ [fromIntegral a]
                            else Left $ l ++
                                        " is not a hex number or out of range"

-- | Get all bytes in a mac address
bytes :: MacAddress -> [Word8]
bytes (MacAddress b0 b1 b2 b3 b4 b5) = [b0, b1, b2, b3, b4, b5]

