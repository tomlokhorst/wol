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
import System.Exit
import Control.Exception

main :: IO ()
main = do
  args <- getArgs
  if length args < 2
   then putStrLn header >> exitFailure
   else do
        let host = args !! 0
            addr = args !! 1
            port = if length args >= 3
                   then read (args !! 2)
                   else 9
        ret <- try $ sendWoLMagicPacket host addr port
               :: IO (Either SomeException ())
        case ret of
          Right _  -> putStrLn "Success"
          Left err -> putStrLn (displayException err) >> exitFailure
  where
    header = "Usage: wol hostname macaddress [portnumber]"


