-- | This module provides access to the Freesound Project, a database of
-- Creative Commons licensed sounds.
--
-- * <http://www.freesound.org/>
--
-- * <http://www.creativecommons.org/>
--
-- > import qualified Network.HTTP.Conduit as HTTP

module Sound.Freesound (
    -- * Freesound API monad
    module Sound.Freesound.API
    -- * Result lists
  , module Sound.Freesound.List
    -- * Users
  , getUserByName
) where

import Sound.Freesound.API (Freesound, runFreesound, APIKey, apiKeyFromString, get, download, downloadToFile)
import Sound.Freesound.List
import Sound.Freesound.User (getUserByName)
