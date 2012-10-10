module Sound.Freesound.Version2.Search (
    SortMethod(..)
  , SortDirection(..)
  , Sorting(..)
  , SearchRequest(..)
  , SearchResponse(..)
  , toURL
) where

import           Network.Curl (CurlBuffer, CurlOption)
import           Network.URL (URL)
import qualified Network.URL as URL
import           Sound.Freesound (Freesound, Response)
import qualified Sound.Freesound as Freesound
import           Sound.Freesound.Version2
import           Sound.Freesound.Version2.Sound
import           Sound.Freesound.Version2.Sound.Filter (Filter)

data SortMethod    = Duration | Created | Downloads | Rating deriving (Eq, Show)
data SortDirection = Ascending | Descending deriving (Eq, Show)
data Sorting       = Sorting SortMethod SortDirection deriving (Eq, Show)
type Filter        = String

data SearchRequest = SearchRequest {
    query   :: String
  , page    :: Int
  , filters :: [Filter]
  , sort    :: Maybe Sorting
} deriving (Eq, Show)

data SearchResponse = SearchResponse {
    sounds   :: [Properties]
  , previous :: URL
  , next     :: URL
} deriving (Eq, Show)

toURL :: SearchRequest -> APIKey -> URL
toURL req = addParams [("q", query req)] . soundSearchURL

makeRequest :: CurlBuffer b => URL -> [CurlOption] -> Freesound (Response b)
makeRequest url = Freesound.request (URL.exportURL url)
