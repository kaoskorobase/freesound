{-# LANGUAGE CPP, OverloadedStrings #-}
module Sound.Freesound.Sound (
    SoundId
  , FileType(..)
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
) where

import Sound.Freesound.Sound.Type
