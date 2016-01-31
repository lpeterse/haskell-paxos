{-# LANGUAGE TypeFamilies #-}
module UdpTransceiver where

import Control.Monad (void)

import Data.ByteString

import System.Socket
import System.Socket.Family.Inet6

import Transceiver

data UdpTransceiver
   = UdpTransceiver
     { udpSocket :: Socket Inet6 Datagram UDP
     }

instance Transceiver UdpTransceiver where
  type Address UdpTransceiver = SocketAddressInet6
  type Message UdpTransceiver = ByteString
  send t m a = void $ sendTo (udpSocket t) m mempty a
  receive t  = receiveFrom (udpSocket t) 4096 mempty
