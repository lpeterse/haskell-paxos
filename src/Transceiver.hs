{-# LANGUAGE TypeFamilies #-}
module Transceiver where

class Transceiver t where
  type Message t
  type Address t
  send    :: t -> Message t -> Address t -> IO ()
  receive :: t -> IO (Message t, Address t)
