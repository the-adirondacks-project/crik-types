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
import Data.Aeson.Types
import Data.Semigroup (Semigroup ((<>)))
import Data.Text (Text)
import GHC.Generics (Generic)

import Crik.Types.File.Id
import Crik.Types.Internal (NoId(NoId))
import Crik.Types.Video (VideoId)
import Crik.Types.Library (LibraryId)

data File id = File {
  fileId :: id,
  videoId :: VideoId,
  fileUrl :: Text,
  libraryId :: LibraryId,
  fileStorageId :: FileStorageId
} deriving (Generic, Show)

-- From JSON instances
instance ToJSON (File FileId) where
  toJSON File{..} = object [
      "id" .= fileId,
      "videoId" .= videoId,
      "url" .= fileUrl,
      "libraryId" .= libraryId,
      "storageId" .= fileStorageId
    ]
  toEncoding File{..} = pairs (
      "id" .= fileId <>
      "videoId" .= videoId <>
      "url" .= fileUrl <>
      "libraryId" .= libraryId <>
      "storageId" .= fileStorageId
    )

instance ToJSON (File NoId) where
  toJSON File{..} = object [
      "videoId" .= videoId,
      "url" .= fileUrl,
      "libraryId" .= libraryId,
      "storageId" .= fileStorageId
    ]
  toEncoding File{..} = pairs (
      "videoId" .= videoId <>
      "url" .= fileUrl <>
      "libraryId" .= libraryId <>
      "storageId" .= fileStorageId
    )

-- From JSON instances
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
