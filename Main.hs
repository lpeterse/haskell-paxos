{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
module Main where

import Control.Monad
import Control.Monad.Reader

import Data.Function
import Data.Maybe
import Data.ByteString
import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM

import Paxos
import qualified Slot as Slot

data Message value
   = SlotMessage Slot (Slot.Message value)
   deriving (Show)

type SlotT = ReaderT Slot

data MultiPaxos value
   = MultiPaxos (IM.IntMap (Slot.State value))
   deriving (Show)

class (Monad m) => Transmitter m where
  send      :: Peer -> Message value -> m ()
  receive   :: m (Peer, Message value)
  broadcast :: Message value -> m ()

instance (Transmitter m) => Slot.SlotM (ReaderT Slot m) where
  sendSlot peer message = ReaderT $ \slot-> do
    send peer (SlotMessage slot message)
  receiveSlot   = ReaderT $ \slot-> fix $ \continue-> do
    (peer, SlotMessage s message) <- receive
    if s == slot
      then return (peer, message)
      else continue
  broadcastSlot message  = ReaderT $ \slot-> do
    broadcast (SlotMessage slot message)

instance (PaxosM m) => PaxosM (ReaderT Slot m) where
  getSelf = lift getSelf
  getSize = lift getSize
  getGreatestSlot = lift getGreatestSlot
  getUnique = lift getUnique

instance (Waiting m) => Waiting (ReaderT Slot m) where
  waitRandomInterval = lift waitRandomInterval

commit :: (PaxosM m, Transmitter m) => value -> m ()
commit value = do
  getGreatestSlot >>= claim . succ
  where
    claim slot = do
      claimed <- runReaderT ( Slot.claim $ Just value ) slot
      unless claimed $ claim ( succ slot )

sync  :: (PaxosM m, Transmitter m) => m ()
sync = do
  getGreatestSlot >>= claim . succ
  where
    claim slot = do
      claimed <- runReaderT ( Slot.claim Nothing ) slot
      unless claimed $ claim ( succ slot )

{-
process :: Quorum -> Peer -> MultiPaxos value -> Message value -> (Maybe (MultiPaxos value), Response (Message value))
process quorum peer (MultiPaxos log) (Message i message) =
  case processPaxosMessage quorum peer paxos message of
    (Nothing, response)      -> ( Nothing,                                    Message i <$> response )
    (Just paxos', response)  -> ( Just $ MultiPaxos $ IM.insert i paxos' log, Message i <$> response )
  where
    paxos = fromMaybe (Debate 0 mempty) $ IM.lookup i log
-}