{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Crik.Types.Library
(
  Library(..)
, LibraryId(..)
, LibraryType(..)
, libraryType
) where

import Data.Aeson (FromJSON(parseJSON), Value(Object), ToJSON(toJSON, toEncoding), (.=), (.:), object, pairs)
import Data.Aeson.Types (typeMismatch)
import Data.Semigroup (Semigroup ((<>)))
import Data.String (IsString(..))
import Data.Text (Text, isPrefixOf)
import GHC.Generics (Generic)

import Crik.TH.DeriveHttpData
import Crik.Types.Internal (NoId(NoId))
import Crik.Types.Library.Id

import Crik.Types.Video (VideoId)

-- TODO: Does this belong? Should this just be stored?
data LibraryType =
  HTTP |
  Directory
  deriving (Show)

instance IsString LibraryType where
  fromString text = case text of
    "http" -> HTTP
    "directory" -> Directory

data Library id = Library {
  libraryId :: id
, libraryUrl :: Text
, libraryName :: Text
} deriving (Generic, Show)

libraryType :: Library a -> LibraryType
libraryType Library{..}
  | "file://" `isPrefixOf` libraryUrl = Directory
  | "http://" `isPrefixOf` libraryUrl = HTTP
  | otherwise = undefined -- TODO: Handle this properly

-- To JSON instances
instance ToJSON (Library NoId) where
  toJSON Library{..} = object ["url" .= libraryUrl, "name" .= libraryName]
  toEncoding Library{..} = pairs ("url" .= libraryUrl <> "name" .= libraryName)

instance ToJSON (Library LibraryId) where
  toJSON Library{..} = object ["id" .= libraryId, "url" .= libraryUrl, "name" .= libraryName]
  toEncoding Library{..} = pairs ("id" .= libraryId <> "url" .= libraryUrl <> "name" .= libraryName)

-- From JSON instances
instance FromJSON (Library LibraryId) where
  parseJSON (Object v) = do
    id <- v .: "id"
    url <- v .: "url"
    name <- v .: "name"
    return (Library (LibraryId id) url name)
  parseJSON invalid = typeMismatch "Library" invalid

instance FromJSON (Library NoId) where
  parseJSON (Object v) = do
    url <- v .: "url"
    name <- v .: "name"
    return $ Library NoId url name
  parseJSON invalid = typeMismatch "Library" invalid
