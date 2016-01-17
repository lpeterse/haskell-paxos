module Main where

import Data.Function
import Data.Maybe
import Data.ByteString
import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM

type    Quorum        = Int
type    Peer          = Int
type    PeerCount     = Int
type    PeerSet       = IS.IntSet
type    Proposal      = Int
type    ProposalMap a = IM.IntMap a

data MultiPaxos value
   = MultiPaxos (IM.IntMap (SinglePaxos value))
   deriving (Show)

data SinglePaxos value
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

class Monad m => Transmitter m where
  receive :: m (PaxosMessage value)
  send :: peer -> PaxosMessage value -> m ()
  broadcast :: PaxosMessage value -> m ()

class Applicative m => Waiting m where
  waitRandomInterval :: m ()

-- | Try to commit a value.
--
--  First tries to get a quorum for a proposal and then issues an accept
--  request.
propose :: (Transmitter m, Waiting m) => PeerCount -> Peer -> Peer -> value -> m value
propose peerCount self peer value = do
  issueProposal self
  where
    issueProposal proposal = do
      broadcast $ Propose proposal
      collectPromises proposal Nothing (IS.singleton self)
    -- We sent a proposal and now wait for a quorum to promise.
    -- When a quorum promised we send an accept request either
    -- with our value or a preserved one from the promises.
    collectPromises proposal conserved peers = fix $ \continue-> do
      message <- receive
      case message of
        Promise promised accepted ->
          case compare promised proposal of
            LT -> continue
            GT -> do
              waitRandomInterval
              issueProposal ( ( promised `div` peerCount ) * peerCount + self )
            EQ -> if IS.member peer peers
              then continue
              else if ( IS.size peers + 1 ) * 2 >= peerCount
                then do
                  broadcast 
                    $ Accept proposal
                    $ maybe value snd conserved
                  waitForConsensus
                else do
                  collectPromises proposal
                    ( case conserved of
                        Nothing    -> accepted
                        Just (c,_) -> case accepted of
                          Nothing    -> conserved
                          Just (a,_) -> if c > a then conserved else accepted
                    )
                    ( IS.insert peer peers )
        Learned v -> return v
        _         -> continue
   -- We sent an accept request and now wait for consensus.
   -- We do not return until we know the value. We rather wait forever.
    waitForConsensus = fix $ \continue-> do
      message <- receive
      case message of
        Learned v -> return v
        _         -> continue

processMessage :: Quorum -> Peer -> MultiPaxos value -> Message value -> (Maybe (MultiPaxos value), Response (Message value))
processMessage quorum peer (MultiPaxos log) (Message i message) =
  case processPaxosMessage quorum peer paxos message of
    (Nothing, response)      -> ( Nothing,                                    Message i <$> response )
    (Just paxos', response)  -> ( Just $ MultiPaxos $ IM.insert i paxos' log, Message i <$> response )
  where
    paxos = fromMaybe (Debate 0 mempty) $ IM.lookup i log

processPaxosMessage :: Quorum -> Peer -> SinglePaxos value -> PaxosMessage value -> (Maybe (SinglePaxos value), Response (PaxosMessage value))
processPaxosMessage quorum peer state message = 
  case message of
    -- Propose means the peer wants us to prepare
    -- and promise not to accept any lower proposals.
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
              -- We and the other peer are accepting although not yet in the peers set (+2!).
              else if IS.size peers + 2 >= quorum
                then ( Just $ Consensus value, Broadcast $ Learned value )
                else ( Just $ Debate proposal $ IM.insert proposal (value, IS.insert peer peers) accepted, Reply message )
    -- Learned means immediate end of the debate.
    -- If we already know about the consensus we do nothing and return the
    -- previous state. In the other case we store the value in the log.
    Learned learned -> case state of
      Consensus _ -> ( Nothing, Silence )
      Debate _ _  -> ( Just $ Consensus learned, Silence )
  where
    highest accepted = case IM.maxViewWithKey accepted of
      Nothing               -> Nothing
      Just ((p, (v, _)), _) -> Just (p, v)
