{-# LANGUAGE CPP, OverloadedStrings #-}
module Sound.Freesound.User (
  User(..)
, Avatar(..)
, getUserByName
) where

import           Control.Monad (join)
import           Data.Aeson (FromJSON(..), Value(..), (.:), (.:?))
import           Data.Text (Text)
import           Sound.Freesound.API (Freesound, Resource, URI, getResource, resourceURI)
import           Sound.Freesound.Bookmark (BookmarkCategory)
import           Sound.Freesound.List (List)
import qualified Sound.Freesound.Pack.Type as Pack
import qualified Sound.Freesound.Sound as Sound
import           Sound.Freesound.Time

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif

data Avatar = Avatar {
    small :: URI
  , medium :: URI
  , large :: URI
  } deriving (Eq, Show)

instance FromJSON Avatar where
  parseJSON (Object v) =
    Avatar <$> v .: "small"
           <*> v .: "medium"
           <*> v .: "large"
  parseJSON _ = fail "Couldn't parse Avatar"

-- | User of the Freesound database.
data User = User {
    url :: URI                              -- ^ The URI for this users' profile on the Freesound website.
  , username :: Text                        -- ^ The username.
  , about :: Maybe Text                     -- ^ The 'about' text of users' profile (if indicated).
  , homepage :: Maybe URI                   -- ^ The URI of users' homepage outside Freesound (if indicated).
  , avatar :: Maybe Avatar                  -- ^ The user's avatar image (if indicated).
  , dateJoined :: UTCTime                   -- ^ The date when the user joined Freesound.
  , numSounds :: Int                        -- ^ The number of sounds uploaded by the user.
  , sounds :: Resource (List Sound.Summary) -- ^ The API URI for this user’s sound collection.
  , numPacks :: Int                         -- ^ The number of packs by the user.
  , packs :: Resource (List Pack.Summary)           -- ^ The API URI for this user’s pack collection.
  , numPosts :: Int                                 -- ^ The number of forum posts by the user.
  , numComments :: Int                              -- ^ The number of comments that user made in other users' sounds.
  , bookmarkCategories :: Resource (List BookmarkCategory)  -- ^ The URI for a list of bookmark categories by the user.
} deriving (Eq, Show)

instance FromJSON User where
  parseJSON (Object v) =
    User
    <$> v .: "url"
    <*> v .: "username"
    <*> (join <$> (v .:? "about"))
    <*> (join <$> (v .:? "homepage"))
    <*> (join <$> (v .:? "avatar"))
    <*> (toUTCTime <$> (v .: "date_joined"))
    <*> v .: "num_sounds"
    <*> v .: "sounds"
    <*> v .: "num_packs"
    <*> v .: "packs"
    <*> v .: "num_posts"
    <*> v .: "num_comments"
    <*> v .: "bookmark_categories"
  parseJSON _ = fail "Couldn't parse User"

-- | Get information about a user by name.
getUserByName :: Text -> Freesound User
getUserByName u = getResource $ resourceURI [ "users", u ] []
