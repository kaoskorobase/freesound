module Sound.Freesound.Test (
  fs
) where

import Sound.Freesound
import System.Environment (getEnv)

fs :: Freesound a -> IO a
fs a = flip runFreesound a =<< apiKeyFromString <$> getEnv "FREESOUND_API_KEY"
