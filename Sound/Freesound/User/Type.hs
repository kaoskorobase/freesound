{-# LANGUAGE OverloadedStrings #-}
module Sound.Freesound.User.Type where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (mzero)
import           Data.Aeson (FromJSON(..), Value(..), (.:))
import           Data.Text (Text)
import qualified Data.Text as T
import           Sound.Freesound.API (Resource, URI)

-- | User of the Freesound database.
class User a where
  -- | The user’s username.
  name :: a -> Text
  -- | The URI for this resource.
  ref :: a -> Resource
  -- | The profile page for the user on the Freesound website.
  url :: a -> URI

-- | User of the Freesound database.
data Summary = Summary {
  user_name :: Text     -- ^ The user’s username.
, user_ref  :: Resource -- ^ The URI for this resource.
, user_url :: URI       -- ^ The profile page for the user on the Freesound website.
} deriving (Eq, Show)

instance User Summary where
  name = user_name
  ref  = user_ref
  url  = user_url

instance FromJSON Summary where
  parseJSON (Object v) =
    Summary
    <$> v .: "username"
    <*> v .: "ref"
    <*> v .: "url"
  parseJSON _ = mzero

data Detail = Detail {
  summary :: Summary                -- ^ Summary.
, sounds :: Resource                -- ^ The API URI for this user’s sound collection.
, packs :: Resource                 -- ^ The API URI for this user’s pack collection.
, firstName :: Maybe Text           -- ^ The user’s first name, possibly empty.
, lastName :: Maybe Text            -- ^ The user’s last name, possibly empty.
, about :: Maybe Text               -- ^ A small text the user wrote about himself.
-- FIXME: homePage :: Maybe Data
, homePage :: Maybe Text            -- ^ The user’s homepage, possibly empty.
, signature :: Maybe Text           -- ^ The user’s signature, possibly empty.
, dateJoined :: Text                -- ^ The date the user joined Freesound.
, bookmarkCategories :: Resource
} deriving (Eq, Show)

instance User Detail where
  name = name . summary
  ref  = ref . summary
  url  = url . summary

textToMaybe :: Text -> Maybe Text
textToMaybe s
  | T.null s  = Nothing
  | otherwise = Just s

instance FromJSON Detail where
  parseJSON j@(Object v) =
    Detail
    <$> parseJSON j
    <*> v .: "sounds"
    <*> v .: "packs"
    <*> (textToMaybe <$> (v .: "first_name"))
    <*> (textToMaybe <$> (v .: "last_name"))
    <*> v .: "about" -- FIXME: Why doesn't return empty string?
    <*> (textToMaybe <$> (v .: "home_page"))
    <*> (textToMaybe <$> (v .: "signature"))
    <*> v .: "date_joined"
    <*> v .: "bookmark_categories"
  parseJSON _ = mzero
