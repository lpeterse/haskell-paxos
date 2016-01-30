{-# LANGUAGE TypeFamilies #-}
module Patchable
  ( Patchable (..)
  ) where

class Patchable value where
  type Patch value
  patch :: Patch value -> value -> value
