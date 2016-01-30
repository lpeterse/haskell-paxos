{-# LANGUAGE TypeFamilies #-}
module Patchable
  ( Patchable (..)
  ) where

class Patchable value where
  type Patch value
  patch :: Patch value -> value -> value

instance Patchable Int where
  type Patch Int = Int
  patch  = const
