{-# LANGUAGE TypeFamilies #-}
module Cluster
  ( ClusterState (..)
  , ClusterNode (..)
  , Cluster (..)
  , new
  ) where

import Patchable

new :: Patchable value => IO (Cluster value)
new  = return Cluster

data Cluster value
   = Cluster

data ClusterState
   = ClusterState
     { clusterNodes :: [Maybe ClusterNode]
     }

data ClusterNode
   = ClusterNode
     { nodeName    :: String
     , nodeAddress :: String
     }
   deriving (Eq)

data ClusterPatch
   = SetNode Int (Maybe ClusterNode)

instance Patchable ClusterState where
  type Patch ClusterState = ClusterPatch
  patch (SetNode i mn) st
    = st { clusterNodes = f (clusterNodes st) }
    where
      f xs = take i xs ++ (mn : drop (i + 1) xs)
