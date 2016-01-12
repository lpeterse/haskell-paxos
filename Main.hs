module Main where

import Data.ByteString
import qualified Data.IntMap as IM

class (Applicative m) => Transmitter m where
  yell :: IndexedMessage -> m ()
  tell :: IndexedMessage -> Member -> m ()

type    Quorum        = Int
type    Proposal      = Int
type    ProposalMap a = IntMap a

data State value peer
   = State
     { stPeerCount :: Int
     , stLog       :: IntMap (PaxosState value)
     }
  deriving (Show)

data PaxosState
   = Consensus
     { value         :: value
     }
   | Debate
     { promised      :: Proposal
     , accepted      :: Maybe (Proposal, value)
     , learned       :: S.Set peer
     }
  deriving (Show)

data Message value
   = Message
     { msgIndex        :: Int
     , msgPaxosMessage :: (PaxosMessage value)
     }
  deriving (Show)

data PaxosMessage value
   = Announce
     { value    :: value
     }
   | Propose
     { proposal :: Proposal
     }
   | Promise
     { proposal :: Proposal
     , accepted :: Maybe (Proposal, value, S.Set peer)
     }
   | Accept -- either by proposer or acceptor (as proxy and confirmation)
     { proposal :: Proposal
     , value    :: value
     }
  deriving (Show)

processMessage :: Transmitter m => State -> Peer -> Message -> m State
processMessage state peer (Message i message) = do
  paxosState   <- pure $ fromMaybe initialState $ IM.lookup i $ stLog state
  mPaxosState' <- alterPaxosState paxosState peer message
  pure $ fromMaybe state $ do
    paxosState <- mPaxosState
    pure $ state { stLog = IM.insert i paxosState $ stLog state }

alterPaxosState :: Transmitter m => PaxosState -> Quorum -> Peer -> PaxosMessage -> m (Maybe PaxosState)
alterPaxosState state peer message = case message of
  -- Announce means immediate end of the discussion.
  -- If we already know about the concensus we do nothing and return the
  -- previous state. In the other case we store the value in the log.
  Announce value ->
    pure $ case state of
      Consensus _ -> Nothing
      Debate _ _  -> Just $ Concensus value
  -- Propose means the peer wants us to prepare and promise not to accept
  -- any lower proposals.
  -- We always send a reply to the peer, but we do not necessarily promise.
  Propose peerProposal ->
    case state of
      -- We tell the sender that concensus has been found.
      -- This is a non-strictly necessary optimisation.
      Consensus value ->
        send peer $ Announce value
        pure Nothing
      -- If the proposal is greater than what we have accepted
      -- we promise to accept nothing lower than the sender's
      -- proposal or the highest proposal we have promised yet.
      Debate promisedProposal acceptedProposal _ -> do
        if peerProposal > promisedProposal
          then do
            send peer $ Promise peerProposal acceptedProposal
            pure  $ Just  $ state { promised = peerProposal, learned = S.empty }
          else do
            send peer $ Promise promisedProposal acceptedProposal
            pure Nothing
  -- We ignore Promise messages.
  Promise _ _ -> pure Nothing
  -- The accept message has a double meaning (Accept and Accepting):
  -- Its either the peer that asks us to accept or the peer was asked to
  -- accept and tells all others including us that he accepts.
  -- It's not necessary to distinguish both cases.
  Accept peerProposal peerValue ->
    case state of
      -- Again: If we know about concensus we tell the peer.
      Consensus value -> do
        send peer $ Announce value
        pure Nothing
      Debate promisedProposal acceptedProposal learnedPeers ->
        case peerProposal `compare` promisedProposal of
          -- An accept request implies the support by a quorum of peers.
          -- Us not having it promised before is unusual, but valid.
          -- We broadcast this accept request to signal that we accepted.
          -- To avoid amplification we broadcast only once per accepted
          -- proposal. Subsequent accept requests for this proposal will only
          -- be replied to the requesting peer.
          GT -> do
            broadcast message
            pure  $ Just $ state { promised = peerProposal
                                 , accepted = Just (peerProposal, peerValue)
                                 , learned  = S.singleton peer
                                 }
          -- An accept request for exactly the proposal that we promised.
          -- We only respond if have not seen a previous accept from this peer
          -- to avoid Ping-Pong of Accept messages.
          LT -> do
            send peer $ Promise promisedProposal acceptedProposal
            pure Nothing
          EQ ->
            if S.null learnedPeers
              then do
                broadcast message
                pure $ Just $ state { accepted = Just (peerProposal, peerValue)
                                    , learned  = S.singleton peer
                                    }
              else if S.member peer learnedPeers
                -- Must be a duplicate - just ignore.
                then pure Nothing
                then if S.size learned + 2 >= quorum
                  then do
                    broadcast $ Announce value
                    pure $ Just $ Consensus value
                  else do
                    send peer message
                    pure $ Just $ state { learned  = S.insert peer learnedPeers }
