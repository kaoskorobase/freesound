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
import           Sound.Freesound.API (Freesound, appendQuery, getResource, resourceURI)
import qualified Sound.Freesound.Bookmark as Bookmark
import qualified Sound.Freesound.Bookmark.Internal as Bookmark
import           Sound.Freesound.Pack (Packs)
import           Sound.Freesound.Search (Pagination)
import           Sound.Freesound.Sound (Sounds)
import           Sound.Freesound.User.Type

-- | Get detailed information about a user.
getUser :: (User a) => a -> Freesound Detail
getUser = getResource . ref

-- | Get information about a user by name.
getUserByName :: Text -> Freesound Detail
getUserByName t = getResource $ resourceURI [ T.pack "people", t ] []

-- | Retrieve a list of a user's bookmark categories.
getBookmarkCategories :: Detail -> Freesound [Bookmark.Category]
getBookmarkCategories = liftM Bookmark.categories . getResource . bookmarkCategories

-- | Retrieve a user's sounds.
-- This is broken: the response doesn't contain the User.
getSounds :: Pagination -> Detail -> Freesound Sounds
getSounds p = getResource . appendQuery p . sounds

-- | Retrieve a user's sounds.

getSounds_ :: Detail -> Freesound Sounds
getSounds_ = getSounds def

-- | Retrieve a user's packs.
getPacks :: Detail -> Freesound Packs
getPacks = getResource . packs
