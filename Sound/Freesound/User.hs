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
import           Sound.Freesound.API (FreesoundT, apiURI, getResource)
import qualified Sound.Freesound.Bookmark as Bookmark
import qualified Sound.Freesound.Bookmark.Internal as Bookmark
import           Sound.Freesound.User.Type

getUser :: (User a, Monad m) => a -> FreesoundT m Detail
getUser = getResource . ref

getUserByName :: Monad m => Text -> FreesoundT m Detail
getUserByName t = apiURI [ T.pack "people", t ] [] >>= getResource

getBookmarkCategories :: Monad m => Detail -> FreesoundT m [Bookmark.Category]
getBookmarkCategories = liftM Bookmark.categories . getResource . bookmarkCategories
