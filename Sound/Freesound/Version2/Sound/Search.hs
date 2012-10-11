module Sound.Freesound.Version2.Search (
    SortMethod(..)
  , SortDirection(..)
  , Sorting(..)
  , SearchResult(..)
  --, toURL
  , searchURI
  , module Sound.Freesound.Version2.Sound.Search.Query
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
import qualified Sound.Freesound.Version2.Sound as Sound
import           Sound.Freesound.Version2.Sound.Search.Filter (Filter)
import           Sound.Freesound.Version2.Sound.Search.Query (Query, ToQueryString(..), include, exclude, (&))

data SortMethod    = Duration | Created | Downloads | Rating deriving (Eq, Show)
data SortDirection = Ascending | Descending deriving (Eq, Show)
data Sorting       = Sorting SortMethod SortDirection deriving (Eq, Show)

instance ToQueryString Sorting where
  toQueryString (Sorting Duration Ascending)   = T.pack "duration_asc"
  toQueryString (Sorting Duration Descending)  = T.pack "duration_desc"
  toQueryString (Sorting Created Ascending)    = T.pack "created_asc"
  toQueryString (Sorting Created Descending)   = T.pack "created_desc"
  toQueryString (Sorting Downloads Ascending)  = T.pack "downloads_asc"
  toQueryString (Sorting Downloads Descending) = T.pack "downloads_desc"
  toQueryString (Sorting Rating Ascending)     = T.pack "rating_asc"  
  toQueryString (Sorting Rating Descending)    = T.pack "rating_desc"

--data Query = Query {
--    query   :: String
--  , page    :: Int
--  , filters :: [Filter]
--  , sorting :: Maybe Sorting
--  -- Support fields parameter by using HList or similar
--  -- Support buffer size parameter (sounds_per_page)
--} deriving (Eq, Show)

searchURI :: Monad m => Query -> [Filter] -> Maybe Sorting -> FreesoundT m URI
searchURI q fs s = do
  u <- apiURI
  return $ addQueryParams [(T.pack "q", toQueryString q)]
         $ addPath "sounds/search" u

data SearchResult = SearchResult {
    sounds   :: [Sound.Info]
  , previous :: Maybe URI
  , next     :: Maybe URI
  } deriving (Eq, Show)

--toURL :: SearchRequest -> APIKey -> URL
--toURL req = addParams [("q", query req)] . soundSearchURL

--makeRequest :: CurlBuffer b => URL -> [CurlOption] -> Freesound (Response b)
--makeRequest url = Freesound.request (URL.exportURL url)
