{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Crik.Types.Library
(
  LibraryId(..)
, Library(..)
) where

import Data.Aeson (FromJSON(parseJSON), Value(Object), ToJSON(toJSON, toEncoding), (.=), (.:), object, pairs)
import Data.Aeson.Types (typeMismatch)
import Data.Semigroup (Semigroup ((<>)))
import Data.Text (Text)
import GHC.Generics (Generic)

import Crik.TH.DeriveHttpData
import Crik.Types.Internal (NoId(NoId))
import Crik.Types.Library.Id
import Crik.Types.Video (VideoId)

data Library id = Library { libraryId :: id, videoLibraryUrl :: Text } deriving (Generic, Show)

-- To JSON instances
instance ToJSON (Library NoId) where
  toJSON Library{..} = object ["url" .= videoLibraryUrl]
  toEncoding Library{..} = pairs ("url" .= videoLibraryUrl)

instance ToJSON (Library LibraryId) where
  toJSON Library{..} = object ["id" .= libraryId, "url" .= videoLibraryUrl]
  toEncoding Library{..} = pairs ("id" .= libraryId <> "url" .= videoLibraryUrl)

-- From JSON instances
instance FromJSON (Library LibraryId) where
  parseJSON (Object v) = do
    id <- v .: "id"
    url <- v .: "url"
    return (Library (LibraryId id) url)
  parseJSON invalid = typeMismatch "Library" invalid

instance FromJSON (Library NoId) where
  parseJSON (Object v) = Library NoId <$> v .: "url"
  parseJSON invalid = typeMismatch "Library" invalid
