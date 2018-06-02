{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Crik.Types.Video.Id
(
  VideoId(..)
) where

import GHC.Generics (Generic)
import Data.Aeson.TH (deriveJSON, defaultOptions, unwrapUnaryRecords)

import Crik.TH.DeriveHttpData

newtype VideoId = VideoId { unVideoId :: Int } deriving (Generic, Show)

$(deriveJSON defaultOptions{unwrapUnaryRecords=True} ''VideoId)
deriveFromHttpData ''VideoId
