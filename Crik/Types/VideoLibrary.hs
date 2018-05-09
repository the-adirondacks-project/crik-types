{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Crik.Types.VideoLibrary
(
  VideoLibraryId(..)
, VideoLibrary(..)
) where

import Data.Aeson (FromJSON(parseJSON), Value(Object), ToJSON(toJSON, toEncoding), (.=), (.:), object, pairs)
import Data.Aeson.TH (deriveJSON, defaultOptions, unwrapUnaryRecords)
import Data.Aeson.Types (typeMismatch)
import Data.Semigroup (Semigroup ((<>)))
import Data.Text (Text)
import GHC.Generics (Generic)

import Crik.Types.Internal (NoId(NoId))
import Crik.Types.Video (VideoId)

newtype VideoLibraryId = VideoLibraryId { unVideoLibraryId :: Int } deriving (Generic)

$(deriveJSON defaultOptions{unwrapUnaryRecords=True} ''VideoLibraryId)

data VideoLibrary id = VideoLibrary { videoLibraryId :: id, videoLibraryUrl :: Text } deriving (Generic)

instance ToJSON (VideoLibrary VideoLibraryId) where
  toJSON VideoLibrary{..} = object ["id" .= videoLibraryId, "url" .= videoLibraryUrl]
  toEncoding VideoLibrary{..} = pairs ("id" .= videoLibraryId <> "url" .= videoLibraryUrl)

instance FromJSON (VideoLibrary VideoLibraryId) where
  parseJSON (Object v) = do
    id <- v .: "id"
    url <- v .: "url"
    return (VideoLibrary (VideoLibraryId id) url)
  parseJSON invalid = typeMismatch "VideoLibrary" invalid

instance FromJSON (VideoLibrary NoId) where
  parseJSON (Object v) = VideoLibrary NoId <$> v .: "url"
  parseJSON invalid = typeMismatch "VideoLibrary" invalid
