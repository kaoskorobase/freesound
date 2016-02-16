{-# LANGUAGE CPP, OverloadedStrings #-}
module Sound.Freesound.Sound (
    SoundId
  , FileType(..)
  , Sound(..)
  , Summary
  , Detail
  , Sounds
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

  , search
  , search_
  , getSimilar
  , getSimilar_
) where

import           Data.Default (def)
import qualified Data.Text as T
import           Network.HTTP.Types.QueryLike (toQuery, toQueryValue)
import           Sound.Freesound.List (List)
import           Sound.Freesound.Search (Filters, Pagination, Query, Sorting)
import           Sound.Freesound.Sound.Type
import           Sound.Freesound.API (Freesound, appendQuery, getResource, resourceURI)

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif

type Sounds = List Summary

search :: Pagination -> Sorting -> Filters -> Query -> Freesound Sounds
search p s fs q =
    getResource
  $ resourceURI
    [ "search", "text" ]
    (toQuery p ++ toQuery [ ("q"::T.Text, toQueryValue q)
                          , ("f", toQueryValue fs)
                          , ("s", toQueryValue s) ])

search_ :: Query -> Freesound Sounds
search_ = search def def def

-- | Search for sounds in a certain coordinate region.
-- geotagged :: ...

-- | Content based search.
-- contentSearch :: ...

-- Missing: distance field in the response
getSimilar :: Pagination -> Detail -> Freesound Sounds
getSimilar p = getResource . appendQuery p . similarSounds

getSimilar_ :: Detail -> Freesound Sounds
getSimilar_ = getSimilar def

-- getAnalysisStats
-- getAnalysisFrames
