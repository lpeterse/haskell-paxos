module Clustered 
  ( Clustered ()
  , readClustered
  , writeClustered
  , modifyClustered
  , patchClustered
  ) where

import Patchable

data Clustered value
   = Clustered
     {

     }

-- | Reads the current value.
--
--   A majority of the cluster might have agreed on a newer value when this
--   operation returns. This is an inevitable logical limitation.
--   
--   Still, the following holds: The value returned has been the most recent one
--   at the moment or after the operation has been entered.
--
--   This operation blocks until a value can be returned.
readClustered :: Clustered value -> IO value
readClustered = 
  undefined

-- | Writes the value.
--
--   The operation blocks until a majority of cluster nodes has accepted it.
--
--   > writeClustered cv v = modifyClustered cv (const v)
writeClustered :: Clustered value -> value -> IO ()
writeClustered cv v =
  modifyClustered cv (const v)

modifyClustered :: Clustered value -> (value -> value) -> IO ()
modifyClustered =
  undefined

-- | Eventually patches the value.
--
--   The most recent value (see `readClustered`) and the `Input value` are used
--   to deterministicly compute a resulting value on all cluster nodes if the
--   modification has been accepted by a majority of cluster nodes.
--
--   The operation returns `Nothing` if `StateMachine.transit` returned `Nothing`,
--   otherwise the resulting value is returned.
patchClustered :: Patchable value => Clustered value -> (value -> Maybe (Patch value)) -> IO (Maybe value)
patchClustered =
  undefined



