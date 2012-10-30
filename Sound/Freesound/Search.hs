{-# LANGUAGE OverloadedStrings #-}
module Sound.Freesound.Search (
    SortMethod(..)
  , SortDirection(..)
  , Sorting
  , unsorted
  , sortedBy
  , Pagination(..)
  , Results
  , results
  , numResults
  , numPages
  , previous
  , next
  , getPrevious
  , getNext
  , module Sound.Freesound.Search.Query
  , Filters
) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (liftM, mzero)
import           Data.Aeson
import qualified Data.ByteString.Char8 as BS
import           Data.Default (Default(..))
import           Network.HTTP.Types.QueryLike (QueryLike(..), QueryValueLike(..))
import           Sound.Freesound.API (FreesoundT, Resource, getResource)
import           Sound.Freesound.Search.Filter (Filters)
import           Sound.Freesound.Search.Query (Query, include, exclude, (&))

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
, resultsPerPage :: Int
} deriving (Eq, Show)

instance Default Pagination where
  def = Pagination 0 15

instance QueryLike Pagination where
  toQuery a = toQuery [ if page def == page a
                        then Nothing
                        else Just (BS.pack "p", show (page a + 1))
                      , if resultsPerPage def == resultsPerPage a
                        then Nothing
                        else Just (BS.pack "sounds_per_page", show (resultsPerPage a)) ]

data Results a = Results {
  results    :: [a]
, numResults :: Int
, numPages   :: Int
, previous   :: Maybe Resource
, next       :: Maybe Resource
} deriving (Eq, Show)

instance FromJSON a => FromJSON (Results a) where
  parseJSON (Object v) =
    Results
    <$> v .: "sounds"
    <*> v .: "num_results"
    <*> v .: "num_pages"
    <*> v .:? "previous"
    <*> v .:? "next"
  parseJSON _ = mzero

getPrevious :: (FromJSON a, Monad m) => Results a -> FreesoundT m (Maybe (Results a))
getPrevious = maybe (return Nothing) (liftM Just . getResource) . previous

getNext :: (FromJSON a, Monad m) => Results a -> FreesoundT m (Maybe (Results a))
getNext = maybe (return Nothing) (liftM Just . getResource) . next

