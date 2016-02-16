{-# LANGUAGE CPP, OverloadedStrings #-}
module Sound.Freesound.Bookmark (
  Bookmark
, sound
, name
, Category
, category
, url
, sounds
, getSounds
) where

import           Control.Monad (mzero)
import           Data.Aeson
import           Data.Text (Text)
import           Sound.Freesound.API (Freesound, Resource, URI, getResource)
import qualified Sound.Freesound.Sound.Type as Sound

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif

data Bookmark = Bookmark {
  sound :: Sound.Summary
, name :: Text
} deriving (Eq, Show)

instance FromJSON Bookmark where
  parseJSON j@(Object v) =
    Bookmark
    <$> parseJSON j
    <*> v .: "bookmark_name"
  parseJSON _ = mzero

data Category = Category {
  category :: Text
, url :: URI
, sounds :: Resource [Bookmark]
} deriving (Eq, Show)

instance FromJSON Category where
  parseJSON (Object v) =
    Category
    <$> v .: "name"
    <*> v .: "url"
    <*> v .: "sounds"
  parseJSON _ = mzero

getSounds :: Category -> Freesound [Bookmark]
getSounds = getResource . sounds
