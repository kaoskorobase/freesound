{-# LANGUAGE OverloadedStrings #-}
module Sound.Freesound.Pack.Type where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (mzero)
import           Data.Aeson (FromJSON(..), Value(..), (.:))
import           Data.Text (Text)
import           Sound.Freesound.API (URI)
import qualified Sound.Freesound.User.Type as User

data Pack = Pack {
  ref :: URI                   -- ^ The URI for this resource.
, url :: URI                   -- ^ The URL for this pack’s page on the Freesound website.
, sounds :: URI                -- ^ The API URI for the pack’s sound collection.
, user :: User.Summary         -- ^ A JSON object with the user’s username, url, and ref.
, name :: Text                 -- ^ The pack’s name.
, created :: Text              -- ^ The date when the pack was created.
, numDownloads :: Integer      -- ^ The number of times the pack was downloaded. 
} deriving (Eq, Show)

instance FromJSON Pack where
  parseJSON (Object v) =
    Pack
    <$> v .: "ref"
    <*> v .: "url"
    <*> v .: "sounds"
    <*> v .: "user"
    <*> v .: "name"
    <*> v .: "created"
    <*> v .: "num_downloads"
  parseJSON _ = mzero
