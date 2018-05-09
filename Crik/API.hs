{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Crik.API
(
  API
, VideoAPI
, VideoLibraryAPI
, GetNewFilesInVideoLibrary
, GetAllFilesInVideoLibrary
) where

import Data.Text (Text)
import Servant.API (Capture, Get, JSON, Post, Put, ReqBody, (:>), (:<|>)((:<|>)))

import Crik.Types.Internal (NoId)
import Crik.Types.Video (Video, VideoId)
import Crik.Types.VideoFile (VideoFile, VideoFileId)
import Crik.Types.VideoLibrary (VideoLibrary, VideoLibraryId)

type API = "api" :> (
    VideoAPI :<|> VideoLibraryAPI
  )

type VideoAPI =
  GetVideos :<|>
  CreateVideo :<|>
  UpdateVideo :<|>
  GetVideo :<|>
  Videos :> CaptureVideoId :> GetFiles :<|>
  Videos :> CaptureVideoId :> GetFile :<|>
  GetFiles :<|>
  GetFile :<|>
  CreateVideoFile

type Videos = "videos"
type CaptureVideoId = Capture "videoId" Int

type GetVideos = Videos :> Get '[JSON] [Video VideoId]
type GetVideo = Videos :> CaptureVideoId :> Get '[JSON] (Video VideoId)
type CreateVideo = Videos :> ReqBody '[JSON] (Video NoId) :> Post '[JSON] (Video VideoId)
type UpdateVideo = Videos :> CaptureVideoId :> ReqBody '[JSON] (Video NoId) :> Put '[JSON] (Video VideoId)

type Files = "files"
type CaptureFileId = Capture "videoFileId" Int

type GetFiles = Files :> Get '[JSON] [VideoFile VideoFileId]
type GetFile = Files :> CaptureFileId :> Get '[JSON] (VideoFile VideoFileId)
type CreateVideoFile = Files :> ReqBody '[JSON] (VideoFile NoId) :> Post '[JSON] (VideoFile VideoFileId)

type VideoLibraryAPI =
  GetVideoLibraries :<|>
  GetVideoLibrary :<|>
  GetNewFilesInVideoLibrary :<|>
  GetAllFilesInVideoLibrary :<|>
  CreateVideoLibrary :<|>
  UpdateVideoLibrary

type VideoLibraries = "video_libraries"
type CaptureVideoLibraryId = Capture "videoLibraryId" Int

type GetVideoLibraries = VideoLibraries :> Get '[JSON] [VideoLibrary VideoLibraryId]
type GetVideoLibrary = VideoLibraries :> CaptureVideoLibraryId :> Get '[JSON] (VideoLibrary VideoLibraryId)
type GetNewFilesInVideoLibrary = VideoLibraries :> CaptureVideoLibraryId :> "new_files" :> Get '[JSON] [Text]
type GetAllFilesInVideoLibrary = VideoLibraries :> CaptureVideoLibraryId :> "all_files" :> Get '[JSON] [Text]
type CreateVideoLibrary = VideoLibraries :> ReqBody '[JSON] (VideoLibrary NoId) :>
  Post '[JSON] (VideoLibrary VideoLibraryId)
type UpdateVideoLibrary = VideoLibraries :> CaptureVideoLibraryId :> ReqBody '[JSON] (VideoLibrary NoId) :>
  Put '[JSON] (VideoLibrary VideoLibraryId)
