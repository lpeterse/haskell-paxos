module Main where

import Data.ByteString
import qualified Data.IntMap as IM

class (Applicative m) => Transmitter m where
  yell :: IndexedMessage -> m ()
  tell :: IndexedMessage -> Member -> m ()

newtype Member = Member Int
newtype Proposal = Proposal Int

data Paxos value
   = Paxos
     { memberCount :: Int
     , log         :: Log
     }
  deriving (Show)

data Log
   = Log
     { entries :: IntMap (LogEntry value)
     }
  deriving (Show)

data LogEntry
   = Concensus value
   | Discussion
     { promised   :: Proposal
     , accepted   :: Maybe (Proposal, value)
     , acceptance :: [Member]
     }
  deriving (Show)

data IndexedMessage value
   = IndexedMessage
     { index   :: Int
     , message :: (Message value)
     }
  deriving (Show)

data Message value
   = Announce
     { value    :: value
     }
   | Propose
     { proposal :: Proposal
     }
   | Promise
     { proposal :: Proposal
     , accepted :: Maybe (Proposal, value)
     }
   | Accept -- either by proposer or acceptor (as proxy and confirmation)
     { proposal :: Proposal
     , value    :: value
     }
  deriving (Show)

processMessage :: Transmitter m => Paxos -> Member -> IndexedMessage -> m Paxos
processMessage st member imsg = do
  case imsg of
    IndexedMessage i msg -> do
      let reply = tell member . IndexedMessage i
      me <- case msg of
        -- Announce means immediate end of the debate.
        -- If we already know about the concensus we do nothing and return the
        -- previous state. In the other case we store the value in the log.
        Announce announcedValue -> do
          case IM.lookup i (log st) of
            Just (Concensus _) -> pure $ Nothing
            _                  -> pure $ Just $ Concensus announcedValue
        Propose currentProposal -> do
          case IM.lookup i (log st) of
            -- We tell the sender that concensus has been found.
            -- This is a non-strictly necessary optimisation.
            Just (Concensus v) -> do
              reply $ Announce v
              pure  $ Nothing
            Just n@(Discussion _ _ _) -> do
              case accepted n of
                -- If we have nothing accepted yet, we can promise.
                -- We promise the current proposal or the memorized one
                -- depending on which is higher.
                Nothing -> do
                  if currentProposal > promisedProposal
                    then do
                      reply $ Promise currentProposal (accepted n)
                      pure  $ Just  $ n { promised = currentProposal }
                    else do
                      reply $ Promise promisedProposal (accepted n)
                      pure  $ Nothing
                -- If we have accepted something we need to check whether
                -- the proposal is eventually obsolete.
                Just a@(acceptedProposal, acceptedValue) -> do
                  if currentProposal > acceptedProposal
                    -- If the proposal is greater than what we have accepted
                    -- we promise to accept nothing lower than the sender's
                    -- proposal or the highest proposal we have promised yet.
                    then if currentProposal > promisedProposal
                      then do
                        reply $ Promise currentProposal (accepted n)
                        pure  $ Just  $ n { promised = currentProposal }
                      else do
                        reply $ Promise promisedProposal (accepted n)
                        pure  $ Nothing
                    -- It the proposal is lower or equal than what we have
                    -- already accepted, we just use the `Accept` message to
                    -- tell the sender although we are not the original proposer.
                    else do
                      reply $ Accept acceptedProposal acceptedValue
                      pure $ Nothing
        Promise propsalPromisedByPeer acceptedByPeer -> do
          case IM.lookup i (log st) of
            Nothing -> case acceptedByPeer of
              Nothing -> do
                pure ()
              Just (proposalAcceptByPeer, valueAcceptedByPeer) -> do
                
            Just (Concensus value) -> do
              reply $ Announce value
            Just (Discussion proposalPromisedByUs acceptedByUs accepting) -> do
        Accept proposalAcceptedByPeer valueAcceptedByPeer -> do
          case IM.lookup i (log st) of
            Nothing -> do
              reply $ msg
              save  $ Discussion
                      { promised  = proposalAcceptedByPeer
                      , accepted  = Just (proposalAcceptedByPeer, valueAcceptedByPeer)
                      , accepting = S.singleton peer
                      }
            Just (Concensus value) -> do
              reply $ Announce value
            Just discussion@(Discussion proposalPromisedByUs acceptedByUs accepting) -> do
              case acceptedByUs of
                Just (proposalAcceptedByUs, valueAcceptedByUs) -> do
                  case proposalAcceptedByPeer `compare` proposalAcceptedByUs of
                    -- Tell everyone that we have accepted the new proposal.
                    GT -> do
                      yell  $ msg
                      save  $ Discussion
                              { promised  = proposalAcceptedByPeer
                              , accepted  = Just (proposalAcceptedByPeer, valueAcceptedByPeer)
                              , accepting = S.singleton peer
                              }
                    -- We already told everyone that we accepted the value.
                    -- Telling others again would lead to PING-PONG, so we keep quiet.
                    -- We just memorize the the peer's acceptance and eventually
                    -- detect concensus.
                    EQ -> do
                      let accepting' = peer `S.insert` accepting
                      if isMajority accepting'
                        then do
                          yell  $ Announce valueAcceptedByUs
                          save  $ Concensus valueAcceptedByUs
                        else do
                          save  $ discussion { accepting = accepting' }
                    -- Tell peer which higher value we accepted and thereby ask him to
                    -- accept it too.
                    LT -> do
                      reply $ Accept proposalAcceptedByUs valueAcceptedByUs
                Nothing -> do
                  reply $ msg
                  pure  $ Just
                        $ Discussion
                          { accepted  = Just (proposalAcceptedByPeer, valueAcceptedByPeer)
                          , accepting = self `S.insert` remote `S.insert` S.empty
                          }
  where
    isMajority acceptingPeers = (S.size acceptingPeers + 1) * 2 > memberCount st
