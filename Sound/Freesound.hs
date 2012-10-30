-- | This module provides access to the Freesound Project, a database of
-- Creative Commons licensed sounds.
--
-- * <http://www.freesound.org/>
--
-- * <http://www.creativecommons.org/>
--
-- > import qualified Network.HTTP.Conduit as HTTP

module Sound.Freesound
(
  -- * Searching
  module FS
  -- * Users
, getUser
, getUserByName
, getBookmarkCategories
  -- * Sounds
, search
, search_
, getSimilar
, getSimilar_
  -- * Freesound API monad
, FreesoundT
, runFreesoundT
)
where

import Sound.Freesound.API (FreesoundT, runFreesoundT)
import Sound.Freesound.Search as FS
import Sound.Freesound.Search.Query as FS
import Sound.Freesound.User (getUser, getUserByName, getBookmarkCategories)
import Sound.Freesound.Sound (search, search_, getSimilar, getSimilar_)