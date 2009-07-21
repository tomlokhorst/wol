-------------------------------------------------------------------------------
-- |
-- Module      : Network.WoL
-- Copyright   : (c) 2009, Tom Lokhorst
-- License     : BSD3
--
-- Maintainer  : Tom Lokhorst <tom@lokhorst.eu>
-- Stability   : Experimental
--
-- Functions for sending a Wake on LAN Magic Packet.
--
-------------------------------------------------------------------------------
module Network.WoL
  ( sendWoLMagicPacket
  , send
  ) where

import Network.MacAddress

import Data.Char
import Network.BSD
import Network.Socket hiding (send)

-- | User friendly wrapper around `send` function.
sendWoLMagicPacket :: String -> String -> Int -> IO ()
sendWoLMagicPacket host addr port = do
  he <- getHostByName host
  let ha = hostAddress he
      ma = parse addr
  send ha ma (fromIntegral port)

-- | Send a magic packet to the specified location.
send :: HostAddress -> MacAddress -> PortNumber -> IO ()
send host addr port = do
  s <- socket AF_INET Datagram udp
  setSocketOption s Broadcast 1
  let sockAddr = SockAddrInet port host
  Network.Socket.sendTo s (magicPacket addr) sockAddr
  sClose s

-- | Construct a magic packet based on `MacAddress`.
magicPacket :: MacAddress -> String
magicPacket addr =
  let prefix = replicate 6 '\255'
      addr'  = map (chr . fromIntegral) (bytes addr)
  in prefix ++ concat (replicate 16 addr')

udp :: ProtocolNumber
udp = 17

