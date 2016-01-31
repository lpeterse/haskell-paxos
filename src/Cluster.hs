{-# LANGUAGE TypeFamilies #-}
module Cluster
  ( ClusterState (..)
  , ClusterNode (..)
  , Node (..)
  , new
  ) where

import Patchable
import Transceiver

new :: (Patchable v, Transceiver t) => t -> IO (ClusterNode v t)
new t = return (ClusterNode undefined t)

-- | A cluster node instance.
data (Transceiver t) => ClusterNode value t
   = ClusterNode
     { clusterState       :: ClusterState
     , clusterTransceiver :: t
     }

data ClusterState
   = ClusterState
     { clusterNodes :: [Node]
     }

data Node
   = Node
     { nodeName    :: String
     , nodeAddress :: String
     }
   deriving (Eq)

data ClusterPatch
   = SetNode Int Node

instance Patchable ClusterState where
  type Patch ClusterState = ClusterPatch
  patch (SetNode i mn) st
    = st { clusterNodes = f (clusterNodes st) }
    where
      f xs = take i xs ++ (mn : drop (i + 1) xs)
