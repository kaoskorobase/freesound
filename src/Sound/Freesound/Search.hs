{-# LANGUAGE CPP, OverloadedStrings #-}
module Sound.Freesound.Search (
    SortMethod(..)
  , SortDirection(..)
  , Sorting
  , unsorted
  , sortedBy
  , Pagination(..)
  , module Sound.Freesound.Search.Filter
  , module Sound.Freesound.Search.Numerical
  , module Sound.Freesound.Search.Query
  , search
  , search_
) where

import qualified Data.ByteString.Char8 as BS
import           Data.Default (Default)
import           Sound.Freesound.Search.Filter
import           Sound.Freesound.Search.Numerical
import           Sound.Freesound.Search.Query
import qualified Data.ByteString as B
import           Data.Default (def)
import qualified Data.Text as T
import           Network.HTTP.Types.QueryLike (QueryLike(..), QueryValueLike(..))
import           Sound.Freesound.List (List)
import           Sound.Freesound.Sound.Type
import           Sound.Freesound.API (Freesound, get, resourceURI)

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif

data SortMethod    = Duration | Created | Downloads | Rating deriving (Eq, Show)
data SortDirection = Ascending | Descending deriving (Eq, Show)
data Sorting       = Unsorted | SortedBy SortMethod SortDirection deriving (Eq, Show)

unsorted :: Sorting
unsorted = Unsorted

sortedBy :: SortMethod -> SortDirection -> Sorting
sortedBy = SortedBy

instance Default Sorting where
  def = unsorted

instance QueryValueLike Sorting where
  toQueryValue Unsorted                        = Nothing
  toQueryValue (SortedBy Duration Ascending)   = Just $ BS.pack "duration_asc"
  toQueryValue (SortedBy Duration Descending)  = Just $ BS.pack "duration_desc"
  toQueryValue (SortedBy Created Ascending)    = Just $ BS.pack "created_asc"
  toQueryValue (SortedBy Created Descending)   = Just $ BS.pack "created_desc"
  toQueryValue (SortedBy Downloads Ascending)  = Just $ BS.pack "downloads_asc"
  toQueryValue (SortedBy Downloads Descending) = Just $ BS.pack "downloads_desc"
  toQueryValue (SortedBy Rating Ascending)     = Just $ BS.pack "rating_asc"
  toQueryValue (SortedBy Rating Descending)    = Just $ BS.pack "rating_desc"

data Pagination = Pagination {
  page :: Int
, pageSize :: Int
} deriving (Eq, Show)

instance Default Pagination where
  def = Pagination 0 15

instance QueryLike Pagination where
  toQuery a = toQuery [ if page def == page a
                        then Nothing
                        else Just (BS.pack "page", show (page a + 1))
                      , if pageSize def == pageSize a
                        then Nothing
                        else Just (BS.pack "page_size", show (pageSize a)) ]

search :: Pagination -> Sorting -> Filters -> Query -> Freesound (List Summary)
search p s fs q =
    get
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
