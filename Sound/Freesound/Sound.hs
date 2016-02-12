module Sound.Freesound.Sound (
  SoundId
, FileType(..)
, Sound(..)
, Summary
, Detail
, Sounds
, samplerate
, bitdepth
, filesize
, bitrate
, channels
, description
, license
, created
, numComments
, numDownloads
, numRatings
, avgRating
, geotag

, search
, search_
, getSimilar
, getSimilar_
) where

import           Control.Applicative (pure, (<*>))
import           Data.Default (def)
import qualified Data.Text as T
import           Network.HTTP.Types.QueryLike (toQuery, toQueryValue)
import           Sound.Freesound.List (List)
import           Sound.Freesound.Search (Filters, Pagination, Query, Sorting)
import           Sound.Freesound.Sound.Type
import           Sound.Freesound.API (Freesound, appendQuery, getResource, resourceURI)

type Sounds = List Summary

search :: Pagination -> Sorting -> Filters -> Query -> Freesound Sounds
search p s fs q =
    getResource
  $ resourceURI
    [ T.pack "sounds", T.pack "search" ]
    (toQuery p ++ toQuery [ pure (,) <*> pure "q" <*> toQueryValue q
                          , pure (,) <*> pure "f" <*> toQueryValue fs
                          , pure (,) <*> pure "s" <*> toQueryValue s ])

search_ :: Query -> Freesound Sounds
search_ = search def def def

-- | Search for sounds in a certain coordinate region.
-- geotagged :: ...

-- | Content based search.
-- contentSearch :: ...

-- Missing: distance field in the response
getSimilar :: (Sound a) => Pagination -> a -> Freesound Sounds
getSimilar p = getResource . appendQuery p . similarity

getSimilar_ :: (Sound a) => a -> Freesound Sounds
getSimilar_ = getSimilar def

-- getAnalysisStats
-- getAnalysisFrames
