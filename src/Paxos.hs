module Paxos where

import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM

type    Quorum        = Int
type    Slot          = Int
type    Peer          = Int
type    PeerCount     = Int
type    PeerSet       = IS.IntSet
type    Proposal      = Int
type    ProposalMap a = IM.IntMap a
type    Unique        = Int

data Response message
   = Silence
   | Reply message
   | Broadcast message
   deriving (Show)

instance Functor Response where
  fmap _ Silence       = Silence
  fmap f (Reply x)     = Reply (f x)
  fmap f (Broadcast x) = Broadcast (f x)

class Monad m => Waiting m where
  waitRandomInterval :: m ()

class (Monad m, Waiting m) => PaxosM m where
  getSelf         :: m Peer
  getSize         :: m Int
  getGreatestSlot :: m Slot
  getUnique       :: m Unique
