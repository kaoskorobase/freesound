module Sound.Freesound.Test (
    fs
  , anySatisfy
  , allSatisfy
) where

import Data.Aeson (FromJSON)
import Sound.Freesound
import System.Environment (getEnv)

fs :: Freesound a -> IO a
fs a = flip runFreesound a =<< apiKeyFromString `fmap` getEnv "FREESOUND_API_KEY"

anySatisfy :: (FromJSON a) => (a -> Bool) -> List a -> Freesound Bool
anySatisfy p l = do
  if any p (elems l)
    then return True
    else do
      n <- getNext l
      case n of
        Nothing -> return False
        Just l' -> anySatisfy p l'

allSatisfy :: (FromJSON a) => (a -> Bool) -> List a -> Freesound Bool
allSatisfy p l = do
  if all p (elems l)
    then do
      n <- getNext l
      case n of
        Nothing -> return True
        Just l' -> allSatisfy p l'
    else return False
