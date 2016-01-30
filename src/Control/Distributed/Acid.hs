module Control.Distributed.Acid
  ( Acid ()
  , readAcid
  , writeAcid
  , modifyAcid
  , patchAcid
  ) where

import Patchable

data Acid value

-- | Reads the value.
--
--   A majority of the cluster might have agreed on a newer value when this
--   operation returns. This is an inevitable logical limitation.
--
--   Still, the following holds: The value returned has been the most recent one
--   at the moment or after the operation has been entered.
--
--   This operation blocks until a value can be returned.
readAcid :: Acid value -> IO value
readAcid =
  undefined

-- | Writes the value.
--
--   The operation blocks until a majority of cluster nodes has accepted it.
--
--   > writeAcid cv v = modifyAcid cv (const v)
writeAcid :: Acid value -> value -> IO ()
writeAcid cv v =
  modifyAcid cv (const v)

modifyAcid :: Acid value -> (value -> value) -> IO ()
modifyAcid =
  undefined

-- | Eventually patches the value.
--
--   The most recent value (see `readAcid`) and the `Input value` are used
--   to deterministicly compute a resulting value on all cluster nodes if the
--   modification has been accepted by a majority of cluster nodes.
--
--   The operation returns `Nothing` if `StateMachine.transit` returned `Nothing`,
--   otherwise the resulting value is returned.
patchAcid :: Patchable value => Acid value -> (value -> Maybe (Patch value)) -> IO (Maybe value)
patchAcid =
  undefined
