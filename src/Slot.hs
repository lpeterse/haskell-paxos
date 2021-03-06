module Slot where

import Data.Function
import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM

import Paxos

data State     value
   = Consensus value
   | Debate    Proposal (ProposalMap (value, PeerSet))
   deriving (Show)

data Message   value
   = Propose   Proposal
   | Promise   Proposal (Maybe (Proposal, value))
   | Accept    Proposal value
   | Learned   value
   deriving (Show)

class SlotM m where
  sendSlot      :: Peer -> Message value -> m ()
  receiveSlot   :: m (Peer, Message value)
  broadcastSlot :: Message value -> m ()

query :: (PaxosM m, SlotM m) => m (Maybe value)
query = do
  undefined

claim :: (PaxosM m, SlotM m) => value -> m Bool
claim value = do
  unique       <- getUnique
  (unique', _) <- propose (unique, value)
  return (unique == unique')

-- | Try to commit a value.
propose :: (PaxosM m, SlotM m) => value -> m value
propose value = do
  getSelf >>= issueProposal
  where
    issueProposal proposal = do
      self <- getSelf
      broadcastSlot $ Propose proposal
      collectPromises proposal Nothing (IS.singleton self)
    -- We sent a proposal and now wait for a quorum to promise.
    -- When a quorum promised we send an accept request either
    -- with our value or a preserved one from the promises.
    collectPromises proposal conserved peers = fix $ \continue-> do
      self <- getSelf
      size <- getSize
      (peer, message) <- receiveSlot
      case message of
        Promise promised accepted ->
          case compare promised proposal of
            LT -> continue
            GT -> do
              waitRandomInterval
              issueProposal ( ( promised `div` size ) * size + self )
            EQ -> if IS.member peer peers
              then continue
              else if ( IS.size peers + 1 ) * 2 >= size
                then do
                  broadcastSlot
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
      (_, message) <- receiveSlot
      case message of
        Learned v -> return v
        _         -> continue

alterState :: (PaxosM m, SlotM m) => State value -> Peer -> Message value -> m (Maybe (State value))
alterState state peer message = do
  size <- getSize
  case message of
    -- Propose means the peer wants us to prepare
    -- and promise not to accept any lower proposals.
    Propose proposal ->
      case state of
        -- We tell the sender that concensus has been found.
        -- This is a non-strictly necessary optimisation.
        Consensus value -> do
          sendSlot peer $ Learned value
          return $ Nothing
        -- If the proposal is greater than what we have accepted
        -- we promise to accept nothing lower than the sender's
        -- proposal or the highest proposal we have promised yet.
        Debate promised accepted -> do
          if proposal > promised
            then do
              sendSlot peer $ Promise proposal $ highest accepted
              return $ Just $ Debate proposal accepted
            else do
              sendSlot peer $ Promise promised $ highest accepted
              return $ Nothing
    -- We ignore Promise messages.
    -- They are only relevant for proposers.
    Promise _ _ -> do
      return $ Nothing
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
        Consensus v -> do
          sendSlot peer $ Learned v
          return $ Nothing
        -- An entry in the accepted map means that we accepted the value.
        -- That means that we can only learn peers for proposals that we
        -- ourselves accepted.
        Debate promised accepted ->
          case IM.lookup proposal accepted of
            -- If no entry is present we have not yet accepted the value.
            Nothing -> if promised > proposal
              -- We promised not to accept this proposal.
              -- Don't update the state and send the peer a Promise as a reject.
              then do
                sendSlot peer $ Promise promised $ highest accepted
                return Nothing
              -- We accept this proposal as it is equal or higher to what we have promised.
              -- We set our promise state to the proposal and create an entry in the accepted map.
              -- We send a broadcast to signal our accept.
              else do
                broadcastSlot message
                return $ Just $ Debate proposal $ IM.insert proposal (value, IS.singleton peer) accepted
            Just (_, peers) -> if IS.member peer peers
              -- Do not send anything here! Replying would lead to ping-pong.
              then do
                return $ Nothing
              -- We and the other peer are accepting although not yet in the peers set (+2!).
              else if ( IS.size peers + 1 ) * 2 >= size
                then do
                  broadcastSlot $ Learned value
                  return $ Just $ Consensus value
                else do
                  sendSlot peer $ message
                  return $ Just $ Debate proposal $ IM.insert proposal (value, IS.insert peer peers) accepted
    -- Learned means immediate end of the debate.
    -- If we already know about the consensus we do nothing and return the
    -- previous state. In the other case we store the value in the log.
    Learned v -> case state of
      Consensus _ -> do
        return $ Nothing
      Debate _ _  -> do
        return $ Just $ Consensus v
  where
    highest accepted = case IM.maxViewWithKey accepted of
      Nothing               -> Nothing
      Just ((p, (v, _)), _) -> Just (p, v)
