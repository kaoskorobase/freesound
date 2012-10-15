module Sound.Freesound.Version2.Sound.Search (
    SortMethod(..)
  , SortDirection(..)
  , Sorting(..)
  --, toURL
  , searchURI
  , search
  , module Sound.Freesound.Version2.Sound.Search.Query
  , Filters
) where

import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text as T
--import           Network.Curl (CurlBuffer, CurlOption)
import           Network.URI (URI)
import qualified Network.URI as URI
--import           Sound.Freesound (Freesound, Response)
--import qualified Sound.Freesound as Freesound
import           Sound.Freesound.URI
import           Sound.Freesound.Version2.Sound (Sounds)
import qualified Sound.Freesound.Version2.Sound as Sound
import           Sound.Freesound.Version2.Sound.Search.Filter (Filters)
import           Sound.Freesound.Version2.Sound.Search.Query (Query, ToQueryString(..), include, exclude, (&))

data SortMethod    = Duration | Created | Downloads | Rating deriving (Eq, Show)
data SortDirection = Ascending | Descending deriving (Eq, Show)
data Sorting       = Sorting SortMethod SortDirection deriving (Eq, Show)

instance ToQueryString Sorting where
  toQueryString (Sorting Duration Ascending)   = "duration_asc"
  toQueryString (Sorting Duration Descending)  = "duration_desc"
  toQueryString (Sorting Created Ascending)    = "created_asc"
  toQueryString (Sorting Created Descending)   = "created_desc"
  toQueryString (Sorting Downloads Ascending)  = "downloads_asc"
  toQueryString (Sorting Downloads Descending) = "downloads_desc"
  toQueryString (Sorting Rating Ascending)     = "rating_asc"  
  toQueryString (Sorting Rating Descending)    = "rating_desc"

searchURI :: Monad m => Query -> Filters -> Maybe Sorting -> FreesoundT m URI
searchURI q fs s = do
  u <- apiURI "sounds/search"
  return $ addQueryParams [("q", toQueryString q), ("f", toQueryString fs)] u

search :: Monad m => Query -> Filters -> Maybe Sorting -> FreesoundT m Sounds
search q fs s = searchURI q fs s >>= getResource . Resource
