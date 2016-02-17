{-# LANGUAGE CPP, OverloadedStrings #-}
module Sound.Freesound.Sound (
    SoundId(..)
  , FileType(..)
  , GeoTag(..)
  , Sound(..)
  , Summary
  , Detail
  , url
  , description
  , geotag
  , created
  , sound_license
  , fileType
  , channels
  , filesize
  , bitrate
  , bitdepth
  , duration
  , samplerate
  , sound_username
  , pack
  , download
  , bookmark
  , previews
  , images
  , numDownloads
  , avgRating
  , numRatings
  , rate
  , comments
  , numComments
  , comment
  , similarSounds
  -- , analysis
  , analysisStats
  , analysisFrames
  , soundById
  , soundDetail
) where

import qualified Data.Text as T
import           Sound.Freesound.API
import           Sound.Freesound.Sound.Type as Sound

-- | Get a sound by id.
soundById :: SoundId -> Freesound Detail
soundById (SoundId i) = getResource $ resourceURI ["sounds", T.pack (show i)] []

-- | Get detailed sound information from its summary.
soundDetail :: Summary -> Freesound Detail
soundDetail = soundById . Sound.id
