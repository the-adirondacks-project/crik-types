{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Crik.Types.File.Id
(
  FileId(..)
, FileStorageId(..)
) where

import Data.Aeson.TH (deriveJSON, defaultOptions, unwrapUnaryRecords)
import Data.Text (Text)
import GHC.Generics (Generic)

import Crik.TH.DeriveHttpData

newtype FileId = FileId { unFileId :: Int } deriving (Generic, Show)

$(deriveJSON defaultOptions{unwrapUnaryRecords=True} ''FileId)
deriveFromHttpData ''FileId

newtype FileStorageId = FileStorageId { unFileStorageId :: Text } deriving (Generic, Show)

$(deriveJSON defaultOptions{unwrapUnaryRecords=True} ''FileStorageId)
