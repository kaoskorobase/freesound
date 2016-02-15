{-# LANGUAGE OverloadedStrings #-}
module Sound.Freesound.Bookmark.Internal (
  Categories(..)
) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (mzero)
import           Data.Aeson
import           Sound.Freesound.Bookmark

data Categories = Categories {
  numCategories :: Int
, categories :: [Category]
} deriving (Eq, Show)

instance FromJSON Categories where
  parseJSON (Object v) =
    Categories
    <$> v .: "num_results"
    <*> v .: "categories"
  parseJSON _ = mzero
