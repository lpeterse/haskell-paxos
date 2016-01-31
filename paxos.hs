module Main where

import Cluster
import UdpTransceiver
import RestInterface
import System.Socket
import System.Socket.Family.Inet6 as Inet6

main :: IO ()
main  = do
  sock <- socket :: IO (Socket Inet6 Datagram UDP)
  setSocketOption sock (ReuseAddress True)
  bind sock (Inet6.SocketAddressInet6 Inet6.any 24768 mempty 0)
  cluster <- Cluster.new (UdpTransceiver sock) :: IO (ClusterNode Int UdpTransceiver)
  RestInterface.run cluster
