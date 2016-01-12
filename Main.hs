module Main where

import Data.Maybe
import Data.ByteString
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

class (Applicative m) => Transmitter m where
  send      :: Peer -> Message value -> m ()
  broadcast :: PaxosMessage value -> m ()

type    Quorum        = Int
type    Peer          = Int
type    PeerSet       = IS.IntSet
type    Proposal      = Int
type    ProposalMap a = IM.IntMap a

data State value
   = State
     { stPeerCount :: Int
     , stLog       :: IM.IntMap (PaxosState value)
     }
   deriving (Show)

data PaxosState value
   = Consensus value
   | Debate    Proposal (ProposalMap (value, PeerSet))
   deriving (Show)

data Message value
   = Message
     { msgIndex        :: Int
     , msgPaxosMessage :: (PaxosMessage value)
     }
   deriving (Show)

data PaxosMessage value
   = Propose  Proposal
   | Promise  Proposal (Maybe (Proposal, value))
   | Accept   Proposal value
   | Learned  value
   deriving (Show)

data Response message
   = Silence
   | Reply message
   | Broadcast message
   deriving (Show)

instance Functor Response where
  fmap f (Silence)     = Silence
  fmap f (Reply x)     = Reply (f x)
  fmap f (Broadcast x) = Broadcast (f x)

processMessage :: Quorum -> Peer -> State value -> Message value -> (Maybe (State value), Response (Message value))
processMessage quorum peer state (Message i message) =
  case processPaxosMessage quorum peer pState message of
    (Nothing, response)      -> ( Nothing
                                , Message i <$> response)
    (Just pState', response) -> ( Just (state { stLog = IM.insert i pState' $ stLog state })
                                , Message i <$> response)
  where
    pState = fromMaybe (Debate 0 mempty) $ IM.lookup i $ stLog state

processPaxosMessage :: Quorum -> Peer -> PaxosState value -> PaxosMessage value -> (Maybe (PaxosState value), Response (PaxosMessage value))
processPaxosMessage quorum peer state message = case message of
  -- Propose means the peer wants us to prepare and promise not to accept
  -- any lower proposals.
  Propose proposal ->
    case state of
      -- We tell the sender that concensus has been found.
      -- This is a non-strictly necessary optimisation.
      Consensus value ->
        ( Nothing, Reply $ Learned value )
      -- If the proposal is greater than what we have accepted
      -- we promise to accept nothing lower than the sender's
      -- proposal or the highest proposal we have promised yet.
      Debate promised accepted ->
        if proposal > promised
          then ( Just $ Debate proposal accepted, Reply $ Promise proposal $ highest accepted)
          else ( Nothing, Reply $ Promise promised $ highest accepted)
  -- We ignore Promise messages.
  -- They are only relevant for proposers.
  Promise _ _ -> ( Nothing, Silence )
  -- The accept message has a double meaning (Accept and Accepting):
  -- Its either the peer that asks us to accept or the peer was asked to
  -- accept and tells all others including us that he accepts.
  -- It's not necessary to distinguish both cases.
  -- An accept request implies the support by a quorum of peers.
  -- Us not having it promised before is unusual, but valid.
  -- We broadcast this accept request to signal that we accepted.
  -- To avoid amplification we broadcast only once per accepted
  -- proposal. Subsequent accept requests for this proposal will only
  -- be replied to the requesting peer.
  Accept proposal value ->
    case state of
      -- Tell the peer that consensus has been found already.
      Consensus learned ->
        ( Nothing, Reply $ Learned learned )
      -- An entry in the accepted map means that we accepted the value.
      -- That means that we can only learn peers for proposals that we
      -- ourselves accepted.
      Debate promised accepted ->
        case IM.lookup proposal accepted of
          -- If no entry is present we have not yet accepted the value.
          Nothing -> if promised > proposal
            -- We promised not to accept this proposal.
            -- Don't update the state and send the peer a Promise as a reject.
            then ( Nothing, Reply $ Promise promised $ highest accepted )
            -- We accept this proposal as it is equal or higher to what we have promised.
            -- We set our promise state to the proposal and create an entry in the accepted map.
            -- We send a broadcast to signal our accept.
            else ( Just $ Debate proposal $ IM.insert proposal (value, IS.singleton peer) accepted, Broadcast message )
          Just (_, peers) -> if IS.member peer peers
            -- Do not send anything here! Replying would lead to ping-pong.
            then ( Nothing, Silence )
            else if IS.size peers + 2 >= quorum
              then ( Just $ Consensus value, Broadcast $ Learned value )
              else ( Just $ Debate proposal $ IM.insert proposal (value, IS.insert peer peers) accepted, Reply message )
  -- Learned means immediate end of the discussion.
  -- If we already know about the concensus we do nothing and return the
  -- previous state. In the other case we store the value in the log.
  Learned learned -> ( Just $ Consensus learned, Silence )
  where
    highest accepted = case IM.maxViewWithKey accepted of
      Nothing               -> Nothing
      Just ((p, (v, _)), _) -> Just (p, v)
