{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Crik.Types.Video
(
  Video(..)
, VideoId(..)
) where

import Data.Aeson (ToJSON(toJSON, toEncoding), FromJSON(..), (.=), object, pairs)
import Data.Aeson.Types
import Data.Aeson.TH (deriveJSON, defaultOptions, unwrapUnaryRecords)
import Data.Semigroup (Semigroup ((<>)))
import Data.Text (Text)
import GHC.Generics (Generic)

import Crik.Types.Internal (NoId(NoId))

newtype VideoId = VideoId { unVideoId :: Int } deriving (Show, Generic)

$(deriveJSON defaultOptions{unwrapUnaryRecords=True} ''VideoId)

data Video id = Video { videoId :: id , videoName :: Text } deriving (Show, Generic)

instance ToJSON (Video VideoId) where
  toJSON Video{..} = object [
      "id" .= videoId,
      "name" .= videoName
    ]
  toEncoding Video{..} = pairs (
      "id" .= videoId <>
      "name" .= videoName
    )

instance ToJSON (Video NoId) where
  toJSON Video{..} = object [
      "name" .= videoName
    ]
  toEncoding Video{..} = pairs (
      "name" .= videoName
    )

instance FromJSON (Video VideoId) where
  parseJSON (Object v) = do
    id <- v .: "id"
    name <- v .: "name"
    return (Video (VideoId id) name)
  parseJSON invalid = typeMismatch "Video" invalid

instance FromJSON (Video NoId) where
  parseJSON (Object v) = Video NoId <$> v .: "name"
  parseJSON invalid = typeMismatch "Video" invalid
