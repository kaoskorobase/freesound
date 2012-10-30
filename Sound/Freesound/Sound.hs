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
import           Sound.Freesound.Search (Filters, Pagination, Query, Results, Sorting)
import           Sound.Freesound.Sound.Type
import           Sound.Freesound.API (FreesoundT, appendQuery, getResource, resourceURI)

type Sounds = Results Summary

search :: Monad m => Pagination -> Sorting -> Filters -> Query -> FreesoundT m Sounds
search p s fs q =
    getResource
  $ resourceURI
    [ T.pack "sounds", T.pack "search" ]
    (toQuery p ++ toQuery [ pure (,) <*> pure "q" <*> toQueryValue q
                          , pure (,) <*> pure "f" <*> toQueryValue fs
                          , pure (,) <*> pure "s" <*> toQueryValue s ])

search_ :: Monad m => Query -> FreesoundT m Sounds
search_ = search def def def

getSimilar :: (Sound a, Monad m) => Pagination -> a -> FreesoundT m Sounds
getSimilar p = getResource . appendQuery (toQuery p) . similarity 

getSimilar_ :: (Sound a, Monad m) => a -> FreesoundT m Sounds
getSimilar_ = getSimilar def
