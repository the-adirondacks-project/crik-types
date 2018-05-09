{-# LANGUAGE DeriveGeneric #-}

module Crik.Types.Internal
(
  NoId(..)
) where

import GHC.Generics (Generic)

data NoId = NoId deriving (Show, Generic)
