{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Crik.Types.VideoFile
(
  VideoFile(..)
, VideoFileId(..)
, VideoFileStorageId(..)
) where

import Data.Aeson (ToJSON(toJSON, toEncoding), FromJSON(parseJSON), (.=), object, pairs)
import Data.Aeson.TH (deriveJSON, defaultOptions, unwrapUnaryRecords, fieldLabelModifier)
import Data.Aeson.Types
import Data.Semigroup (Semigroup ((<>)))
import Data.Text (Text)
import GHC.Generics (Generic)

import Crik.Types.Internal (NoId(NoId))
import Crik.Types.Video (VideoId)
import Crik.Types.VideoLibrary (VideoLibraryId)

newtype VideoFileId = VideoFileId { unVideoFileId :: Int } deriving (Generic)

$(deriveJSON defaultOptions{unwrapUnaryRecords=True} ''VideoFileId)

newtype VideoFileStorageId = VideoFileStorageId { unVideoFileStorageId :: Text } deriving (Generic)

$(deriveJSON defaultOptions{unwrapUnaryRecords=True} ''VideoFileStorageId)

data VideoFile id = VideoFile {
  videoFileId :: id,
  videoId :: VideoId,
  videoFileUrl :: Text,
  videoLibraryId :: VideoLibraryId,
  videoFileStorageId :: VideoFileStorageId
} deriving (Generic)

instance ToJSON (VideoFile VideoFileId) where
  toJSON VideoFile{..} = object [
      "id" .= videoFileId,
      "videoId" .= videoId,
      "url" .= videoFileUrl,
      "libraryId" .= videoLibraryId,
      "storageId" .= videoFileStorageId
    ]
  toEncoding VideoFile{..} = pairs (
      "id" .= videoFileId <>
      "videoId" .= videoId <>
      "url" .= videoFileUrl <>
      "libraryId" .= videoLibraryId <>
      "storageId" .= videoFileStorageId
    )

instance ToJSON (VideoFile NoId) where
  toJSON VideoFile{..} = object [
      "videoId" .= videoId,
      "url" .= videoFileUrl,
      "libraryId" .= videoLibraryId,
      "storageId" .= videoFileStorageId
    ]
  toEncoding VideoFile{..} = pairs (
      "videoId" .= videoId <>
      "url" .= videoFileUrl <>
      "libraryId" .= videoLibraryId <>
      "storageId" .= videoFileStorageId
    )

instance FromJSON (VideoFile NoId) where
  parseJSON (Object v) = do
    videoId <- v .: "videoId"
    url <- v .: "url"
    libraryId <- v .: "libraryId"
    storageId <- v .: "storageId"
    return (VideoFile NoId videoId url libraryId storageId)
  parseJSON invalid = typeMismatch "VideoFile" invalid
