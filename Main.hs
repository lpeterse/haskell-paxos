module Main where

-- A proposer chooses a new proposal number n and sends a request to
-- each member of some set of acceptors, asking it to respond with:
-- (a) A promise never again to accept a proposal numbered less than
--     n, and
-- (b) The proposal with the highest number less than n that it has
--     accepted, if any

digest :: MonadAcceptor m => Proposal -> m ()
digest (PrepareRequest proposalNumber proposalValue) = do
  promised <- recallPromise
  accepted <- recallAccepted
  case promise of
    Nothing ->
      promise
    Just (Promise promisedNumber promisedValue) ->
      if proposalNumber > promisedNumber then
        promise
      else
        reject

digest (AcceptRequest proposalNumber proposalValue) = do
  promised <- recallPromise
  case promised of
    Nothing ->
      accept
    Just (Promise promisedNumber promisedValue) ->
      if proposalNumber >= promisedNumber then
        accept
      else
        reject




