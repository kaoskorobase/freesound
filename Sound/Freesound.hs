-- | This module provides access to the Freesound Project, a database of
-- Creative Commons licensed sounds.
--
-- * <http://www.freesound.org/>
--
-- * <http://www.creativecommons.org/>
module Sound.Freesound
--(
--    -- * The Freesound monad
--    Freesound
--  , Response
--  , request
--  , withFreesound
--    -- * Error handling
--  , Error(..)
--  , errorString
--    -- * Sample handles
--  , Sample(..)
--    -- * API methods
--  , search
--  , Similarity(..)
--  , searchSimilar
--  , propertiesXML
--  , properties
--  , download
--)
where

import Data.Text (Text)
import qualified Data.Text as T
import Sound.Freesound.Types
import Sound.Freesound.URI
import Sound.Freesound.Conduit

userByName :: Monad m => Text -> FreesoundT m User
userByName t = apiURI [ T.pack "people", t ] [] >>= getResourceURI
