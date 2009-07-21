-------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Copyright   : (c) 2009, Tom Lokhorst
-- License     : BSD3
--
-- Maintainer  : Tom Lokhorst <tom@lokhorst.eu>
-- Stability   : Experimental
--
-- Main module for `wol` executable.
--
-------------------------------------------------------------------------------
module Main where

import Network.WoL

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  if length args < 2
   then putStrLn header
   else do
        let host = args !! 0
            addr = args !! 1
            port = if length args >= 3
                   then read (args !! 2)
                   else 9
        sendWoLMagicPacket host addr port
  where
    header = "Usage: wol hostname macaddress [portnumber]"


