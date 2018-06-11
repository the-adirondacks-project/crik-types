{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Crik.Types.Library.Id
(
  LibraryId(..)
) where

import Data.Aeson.TH (deriveJSON, defaultOptions, unwrapUnaryRecords)
import GHC.Generics (Generic)

import Crik.TH.DeriveHttpData

newtype LibraryId = LibraryId { unLibraryId :: Int } deriving (Generic, Show)

$(deriveJSON defaultOptions{unwrapUnaryRecords=True} ''LibraryId)
deriveFromHttpData ''LibraryId
