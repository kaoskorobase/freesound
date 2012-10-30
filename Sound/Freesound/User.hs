module Sound.Freesound.User (
  User(..)
, Summary
, Detail
, sounds
, packs
, firstName
, lastName
, about
, homePage
, signature
, dateJoined
, bookmarkCategories
, getUser
, getUserByName
, getBookmarkCategories
, getSounds
, getSounds_
, getPacks
) where

import           Control.Monad (liftM)
import           Data.Default (def)
import           Data.Text (Text)
import qualified Data.Text as T
import           Sound.Freesound.API (FreesoundT, appendQuery, getResource, resourceURI)
import qualified Sound.Freesound.Bookmark as Bookmark
import qualified Sound.Freesound.Bookmark.Internal as Bookmark
import           Sound.Freesound.Pack (Packs)
import           Sound.Freesound.Search (Pagination)
import           Sound.Freesound.Sound (Sounds)
import           Sound.Freesound.User.Type

-- | Get detailed information about a user.
getUser :: (User a, Monad m) => a -> FreesoundT m Detail
getUser = getResource . ref

-- | Get information about a user by name.
getUserByName :: Monad m => Text -> FreesoundT m Detail
getUserByName t = getResource $ resourceURI [ T.pack "people", t ] []

-- | Retrieve a list of a user's bookmark categories.
getBookmarkCategories :: Monad m => Detail -> FreesoundT m [Bookmark.Category]
getBookmarkCategories = liftM Bookmark.categories . getResource . bookmarkCategories

-- | Retrieve a user's sounds.
-- This is broken: the response doesn't contain the User.
getSounds :: Monad m => Pagination -> Detail -> FreesoundT m Sounds
getSounds p = getResource . appendQuery p . sounds

-- | Retrieve a user's sounds.

getSounds_ :: Monad m => Detail -> FreesoundT m Sounds
getSounds_ = getSounds def

-- | Retrieve a user's packs.
getPacks :: Monad m => Detail -> FreesoundT m Packs
getPacks = getResource . packs
