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
, GetVideoLibrary
, GetVideoLibraries
, GetNewFilesInVideoLibrary
, GetAllFilesInVideoLibrary
, CreateVideoLibrary
, UpdateVideoLibrary
, GetNewFilesInVideoLibrary
, GetAllFilesInVideoLibrary
) where

import Data.Text (Text)
import Servant.API (Capture, Get, JSON, Post, Put, ReqBody, (:>), (:<|>)((:<|>)))

import Crik.Types.Internal (NoId)
import Crik.Types.Video (Video, VideoId)
import Crik.Types.VideoFile (VideoFile, VideoFileId)
import Crik.Types.VideoLibrary (VideoLibrary, VideoLibraryId)

type CrikAPI = VideoAPI :<|> FileAPI :<|> LibraryAPI
type Version = "api"

type VideoAPI =
  GetVideo :<|>
  GetVideos :<|>
  CreateVideo :<|>
  UpdateVideo :<|>
  GetFilesForVideo

type Videos = "videos"
type CaptureVideoId = Capture "videoId" Int

type GetVideo = Version :> Videos :> CaptureVideoId :> Get '[JSON] (Video VideoId)
type GetVideos = Version :> Videos :> Get '[JSON] [Video VideoId]
type CreateVideo = Version :> Videos :> ReqBody '[JSON] (Video NoId) :> Post '[JSON] (Video VideoId)
type UpdateVideo = Version :> Videos :> CaptureVideoId :> ReqBody '[JSON] (Video NoId) :> Put '[JSON] (Video VideoId)
type GetFilesForVideo = Version :> Videos :> CaptureVideoId :> Get '[JSON] [VideoFile VideoFileId]

type FileAPI =
  GetFile :<|>
  GetFiles :<|>
  CreateFile

type Files = "files"
type CaptureFileId = Capture "videoFileId" Int

type GetFile = Version :> Files :> CaptureFileId :> Get '[JSON] (VideoFile VideoFileId)
type GetFiles = Version :> Files :> Get '[JSON] [VideoFile VideoFileId]
type CreateFile = Version :> Files :> ReqBody '[JSON] (VideoFile NoId) :> Post '[JSON] (VideoFile VideoFileId)

type LibraryAPI =
  GetVideoLibrary :<|>
  GetVideoLibraries :<|>
  CreateVideoLibrary :<|>
  UpdateVideoLibrary :<|>
  GetNewFilesInVideoLibrary :<|>
  GetAllFilesInVideoLibrary

type VideoLibraries = "video_libraries"
type CaptureVideoLibraryId = Capture "videoLibraryId" Int

type GetVideoLibrary = Version :> VideoLibraries :> CaptureVideoLibraryId :> Get '[JSON] (VideoLibrary VideoLibraryId)
type GetVideoLibraries = Version :> VideoLibraries :> Get '[JSON] [VideoLibrary VideoLibraryId]
type CreateVideoLibrary = Version :> VideoLibraries :> ReqBody '[JSON] (VideoLibrary NoId) :>
  Post '[JSON] (VideoLibrary VideoLibraryId)
type UpdateVideoLibrary = Version :> VideoLibraries :> CaptureVideoLibraryId :> ReqBody '[JSON] (VideoLibrary NoId) :>
  Put '[JSON] (VideoLibrary VideoLibraryId)
type GetNewFilesInVideoLibrary = Version :> VideoLibraries :> CaptureVideoLibraryId :> "new_files" :> Get '[JSON] [Text]
type GetAllFilesInVideoLibrary = Version :> VideoLibraries :> CaptureVideoLibraryId :> "all_files" :> Get '[JSON] [Text]
