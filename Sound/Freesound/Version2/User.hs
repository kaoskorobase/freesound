module Sound.Freesound.Version2.User (
    User(..)
) where

import Network.URL (URL)

-- | User of the Freesound database.
data User = User {
    userId   :: Int
  , userName :: String
  , userURL  :: URL
} deriving (Eq, Show)
