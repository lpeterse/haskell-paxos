module Main where

import Cluster
import RestInterface

main :: IO ()
main  = do
  cluster <- Cluster.new :: IO (Cluster Int)
  RestInterface.run cluster
