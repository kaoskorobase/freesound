{-# LANGUAGE OverloadedStrings #-}
module Sound.Freesound.Pack.Type where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (mzero)
import           Data.Aeson (FromJSON(..), Value(..), (.:))
import           Data.Text (Text)
import           Sound.Freesound.API (Resource, URI)
import           Sound.Freesound.List (Elem(..))
import qualified Sound.Freesound.User.Type as User

class Pack a where
  -- | The URI for this resource.
  ref :: a -> Resource
  -- | The URL for this pack’s page on the Freesound website.
  url :: a -> URI
  -- | The API URI for the pack’s sound collection.
  sounds :: a -> Resource
  -- | The pack’s name.
  name :: a -> Text
  -- | The date when the pack was created.
  created :: a -> Text
  -- | The number of times the pack was downloaded.
  numDownloads :: a -> Integer

data Summary = Summary {
  pack_ref :: Resource              -- ^ The URI for this resource.
, pack_url :: URI                   -- ^ The URL for this pack’s page on the Freesound website.
, pack_sounds :: Resource           -- ^ The API URI for the pack’s sound collection.
, pack_name :: Text                 -- ^ The pack’s name.
, pack_created :: Text              -- ^ The date when the pack was created.
, pack_num_downloads :: Integer     -- ^ The number of times the pack was downloaded.
} deriving (Eq, Show)

instance Pack Summary where
  ref = pack_ref
  url = pack_url
  sounds = pack_sounds
  name = pack_name
  created = pack_created
  numDownloads = pack_num_downloads

instance FromJSON Summary where
  parseJSON (Object o) =
    Summary
    <$> o .: "ref"
    <*> o .: "url"
    <*> o .: "sounds"
    <*> o .: "name"
    <*> o .: "created"
    <*> o .: "num_downloads"
  parseJSON _ = mzero

instance Elem Summary where
  elemsFieldName _ = "packs"

data Detail = Detail {
  summary :: Summary
, user :: User.Summary         -- ^ A JSON object with the user’s username, url, and ref.
} deriving (Eq, Show)

instance Pack Detail where
  ref = ref . summary
  url = url . summary
  sounds = sounds . summary
  name = name . summary
  created = created . summary
  numDownloads = numDownloads . summary

instance FromJSON Detail where
  parseJSON v@(Object o) =
    Detail
    <$> parseJSON v
    <*> o .: "user"
  parseJSON _ = mzero
