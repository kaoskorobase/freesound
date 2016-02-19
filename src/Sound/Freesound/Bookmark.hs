{-# LANGUAGE CPP, OverloadedStrings #-}
module Sound.Freesound.Bookmark (
  BookmarkCategory(..)
) where

import           Data.Aeson
import           Data.Text (Text)
import           Sound.Freesound.API (Resource, URI)
import           Sound.Freesound.List (List)
import qualified Sound.Freesound.Sound.Type as Sound

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif

data BookmarkCategory = BookmarkCategory {
    url :: URI
  , name :: Text
  , numSounds :: Int
  , sounds :: Resource (List Sound.Summary)
} deriving (Eq, Show)

instance FromJSON BookmarkCategory where
  parseJSON (Object v) =
    BookmarkCategory
      <$> v .: "url"
      <*> v .: "name"
      <*> v .: "num_sounds"
      <*> v .: "sounds"
  parseJSON _ = fail "Couldn't parse BookmarkCategory"
