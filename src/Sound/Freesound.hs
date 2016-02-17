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
  module Sound.Freesound.Search
, module Sound.Freesound.Search.Query
, module Sound.Freesound.List
  -- * Users
, getUserByName
  -- * Freesound API monad
, module Sound.Freesound.API
)
where

import Sound.Freesound.API (Freesound, runFreesound, APIKey, apiKeyFromString, getResource)
import Sound.Freesound.List
import Sound.Freesound.Search
import Sound.Freesound.Search.Query
import Sound.Freesound.User (getUserByName)
