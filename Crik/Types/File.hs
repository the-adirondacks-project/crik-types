{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Crik.Types.File
(
  File(..)
, FileId(..)
, FileStorageId(..)
) where

import Data.Aeson (ToJSON(toJSON, toEncoding), FromJSON(parseJSON), (.=), object, pairs)
import Data.Aeson.TH (deriveJSON, defaultOptions, unwrapUnaryRecords, fieldLabelModifier)
import Data.Aeson.Types
import Data.Semigroup (Semigroup ((<>)))
import Data.Text (Text)
import GHC.Generics (Generic)

import Crik.TH.DeriveHttpData
import Crik.Types.Internal (NoId(NoId))
import Crik.Types.Video (VideoId)
import Crik.Types.VideoLibrary (VideoLibraryId)

newtype FileId = FileId { unFileId :: Int } deriving (Generic, Show)

$(deriveJSON defaultOptions{unwrapUnaryRecords=True} ''FileId)
deriveFromHttpData ''FileId

newtype FileStorageId = FileStorageId { unFileStorageId :: Text } deriving (Generic, Show)

$(deriveJSON defaultOptions{unwrapUnaryRecords=True} ''FileStorageId)

data File id = File {
  fileId :: id,
  videoId :: VideoId,
  videoFileUrl :: Text,
  videoLibraryId :: VideoLibraryId,
  videoFileStorageId :: FileStorageId
} deriving (Generic, Show)

instance ToJSON (File FileId) where
  toJSON File{..} = object [
      "id" .= fileId,
      "videoId" .= videoId,
      "url" .= videoFileUrl,
      "libraryId" .= videoLibraryId,
      "storageId" .= videoFileStorageId
    ]
  toEncoding File{..} = pairs (
      "id" .= fileId <>
      "videoId" .= videoId <>
      "url" .= videoFileUrl <>
      "libraryId" .= videoLibraryId <>
      "storageId" .= videoFileStorageId
    )

instance ToJSON (File NoId) where
  toJSON File{..} = object [
      "videoId" .= videoId,
      "url" .= videoFileUrl,
      "libraryId" .= videoLibraryId,
      "storageId" .= videoFileStorageId
    ]
  toEncoding File{..} = pairs (
      "videoId" .= videoId <>
      "url" .= videoFileUrl <>
      "libraryId" .= videoLibraryId <>
      "storageId" .= videoFileStorageId
    )

instance FromJSON (File FileId) where
  parseJSON (Object v) = do
    id <- v .: "id"
    videoId <- v .: "videoId"
    url <- v .: "url"
    libraryId <- v .: "libraryId"
    storageId <- v .: "storageId"
    return (File id videoId url libraryId storageId)
  parseJSON invalid = typeMismatch "File" invalid

instance FromJSON (File NoId) where
  parseJSON (Object v) = do
    videoId <- v .: "videoId"
    url <- v .: "url"
    libraryId <- v .: "libraryId"
    storageId <- v .: "storageId"
    return (File NoId videoId url libraryId storageId)
  parseJSON invalid = typeMismatch "File" invalid
