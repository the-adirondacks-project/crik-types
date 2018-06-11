module Crik.Types.FilePath
(
  FilePath(..)
) where

import Data.String (IsString(fromString))
import Prelude hiding (FilePath)

data FilePath = FilePath {
  unFilePath :: String
} deriving (Read, Show, Eq, Ord)

instance IsString FilePath where
  fromString = FilePath
