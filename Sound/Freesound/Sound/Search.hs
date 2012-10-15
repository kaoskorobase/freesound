module Sound.Freesound.Sound.Search (
    SortMethod(..)
  , SortDirection(..)
  , Sorting(..)
  --, toURL
  , searchURI
  , search
  , module Sound.Freesound.Sound.Search.Query
  , Filters
) where

import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text as T
--import           Network.Curl (CurlBuffer, CurlOption)
import           Network.HTTP.Types.QueryLike (QueryValueLike(..))
import           Network.URI (URI)
import qualified Network.URI as URI
--import           Sound.Freesound (Freesound, Response)
--import qualified Sound.Freesound as Freesound
import           Sound.Freesound.URI
import           Sound.Freesound.Types (Sounds)
import           Sound.Freesound.Sound.Search.Filter (Filters)
import           Sound.Freesound.Sound.Search.Query (Query, include, exclude, (&))

data SortMethod    = Duration | Created | Downloads | Rating deriving (Eq, Show)
data SortDirection = Ascending | Descending deriving (Eq, Show)
data Sorting       = Sorting SortMethod SortDirection deriving (Eq, Show)

instance QueryValueLike Sorting where
  toQueryValue (Sorting Duration Ascending)   = toQueryValue "duration_asc"
  toQueryValue (Sorting Duration Descending)  = toQueryValue "duration_desc"
  toQueryValue (Sorting Created Ascending)    = toQueryValue "created_asc"
  toQueryValue (Sorting Created Descending)   = toQueryValue "created_desc"
  toQueryValue (Sorting Downloads Ascending)  = toQueryValue "downloads_asc"
  toQueryValue (Sorting Downloads Descending) = toQueryValue "downloads_desc"
  toQueryValue (Sorting Rating Ascending)     = toQueryValue "rating_asc"  
  toQueryValue (Sorting Rating Descending)    = toQueryValue "rating_desc"

searchURI :: Monad m => Query -> Filters -> Maybe Sorting -> FreesoundT m URI
searchURI q fs s = apiURI [T.pack "sounds", T.pack "search"]
                          [ ((,)"q") `fmap` toQueryValue q
                          , ((,)"f") `fmap` toQueryValue fs
                          , ((,)"s") `fmap` toQueryValue s ]

search :: Monad m => Query -> Filters -> Maybe Sorting -> FreesoundT m Sounds
search q fs s = searchURI q fs s >>= getResource . Resource
