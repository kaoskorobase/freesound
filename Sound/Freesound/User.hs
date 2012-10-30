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
) where

import           Control.Monad (liftM)
import           Data.Text (Text)
import qualified Data.Text as T
import           Sound.Freesound.API (FreesoundT, getResource, resourceURI)
import qualified Sound.Freesound.Bookmark as Bookmark
import qualified Sound.Freesound.Bookmark.Internal as Bookmark
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
