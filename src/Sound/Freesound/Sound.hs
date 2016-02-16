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

  , search
  , search_
) where

import qualified Data.ByteString as B
import           Data.Default (def)
import qualified Data.Text as T
import           Network.HTTP.Types.QueryLike (QueryLike(..), QueryValueLike(..))
import           Sound.Freesound.List (List)
import           Sound.Freesound.Search (Filters, Pagination, Query, Sorting)
import           Sound.Freesound.Sound.Type
import           Sound.Freesound.API (Freesound, getResource, resourceURI)

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif

search :: Pagination -> Sorting -> Filters -> Query -> Freesound (List Summary)
search p s fs q =
    getResource
  $ resourceURI
    [ "search", "text" ]
    (toQuery p ++ toQuery [ pair "query" q
                          , pair "filter" fs
                          , pair "sort" s ])
                          -- TODO: group_by_pack (changes response type)
  where
    pair :: QueryValueLike a => T.Text -> a -> Maybe (T.Text, B.ByteString)
    pair k a = (,) <$> pure k <*> toQueryValue a

search_ :: Query -> Freesound (List Summary)
search_ = search def def def
