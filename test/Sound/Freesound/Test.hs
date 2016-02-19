module Sound.Freesound.Test (
    fs
  , anySatisfy
  , allSatisfy
  , getAll
) where

import Data.Aeson (FromJSON)
import Sound.Freesound
import qualified Sound.Freesound.List as L
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

getAll :: (FromJSON a) => List a -> Freesound [a]
getAll l =
  case L.next l of
    Nothing -> return $ L.elems l
    Just n -> do
      xs <- getAll =<< get n
      return $ L.elems l ++ xs
