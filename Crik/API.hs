{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Crik.API
(
-- Entire API
  CrikAPI
-- API Version
, Version
-- Video API
, VideoAPI
, GetVideo
, GetVideos
, CreateVideo
, UpdateVideo
, GetFilesForVideo
-- Files API
, FileAPI
, GetFile
, GetFiles
, CreateFile
-- Library API
, LibraryAPI
, GetLibrary
, GetLibraries
, CreateLibrary
, UpdateLibrary
, GetNewFilesInLibrary
, GetAllFilesInLibrary
) where

import Data.Text (Text)
import Servant.API (Capture, Get, JSON, Post, Put, ReqBody, (:>), (:<|>)((:<|>)))

import Crik.Types.Internal (NoId)
import Crik.Types.Video (Video, VideoId)
import Crik.Types.File (File, FileId)
import Crik.Types.Library (Library, LibraryId)

type CrikAPI = VideoAPI :<|> FileAPI :<|> LibraryAPI
type Version = "api"

type VideoAPI =
  GetVideo :<|>
  GetVideos :<|>
  CreateVideo :<|>
  UpdateVideo :<|>
  GetFilesForVideo

type Videos = "videos"
type CaptureVideoId = Capture "videoId" VideoId

type GetVideo = Version :> Videos :> CaptureVideoId :> Get '[JSON] (Video VideoId)
type GetVideos = Version :> Videos :> Get '[JSON] [Video VideoId]
type CreateVideo = Version :> Videos :> ReqBody '[JSON] (Video NoId) :> Post '[JSON] (Video VideoId)
type UpdateVideo = Version :> Videos :> CaptureVideoId :> ReqBody '[JSON] (Video (Maybe VideoId))
  :> Put '[JSON] (Video VideoId)
type GetFilesForVideo = Version :> Videos :> CaptureVideoId :> Get '[JSON] [File FileId]

type FileAPI =
  GetFile :<|>
  GetFiles :<|>
  CreateFile

type Files = "files"
type CaptureFileId = Capture "videoFileId" FileId

type GetFile = Version :> Files :> CaptureFileId :> Get '[JSON] (File FileId)
type GetFiles = Version :> Files :> Get '[JSON] [File FileId]
type CreateFile = Version :> Files :> ReqBody '[JSON] (File NoId) :> Post '[JSON] (File FileId)

type LibraryAPI =
  GetLibrary :<|>
  GetLibraries :<|>
  CreateLibrary :<|>
  UpdateLibrary :<|>
  GetNewFilesInLibrary :<|>
  GetAllFilesInLibrary

type Libraries = "libraries"
type CaptureLibraryId = Capture "LibraryId" LibraryId

type GetLibrary = Version :> Libraries :> CaptureLibraryId :> Get '[JSON] (Library LibraryId)
type GetLibraries = Version :> Libraries :> Get '[JSON] [Library LibraryId]
type CreateLibrary = Version :> Libraries :> ReqBody '[JSON] (Library NoId) :>
  Post '[JSON] (Library LibraryId)
type UpdateLibrary = Version :> Libraries :> CaptureLibraryId :>
  ReqBody '[JSON] (Library NoId) :> Put '[JSON] (Library LibraryId)
type GetNewFilesInLibrary = Version :> Libraries :> CaptureLibraryId :> "new_files"
  :> Get '[JSON] [Text]
type GetAllFilesInLibrary = Version :> Libraries :> CaptureLibraryId :> "all_files"
  :> Get '[JSON] [Text]
