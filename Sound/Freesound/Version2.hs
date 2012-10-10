module Sound.Freesound.Version2 (
    addPath
  , addParams
  , APIKey
  , apiURL
  , soundSearchURL
  , soundURL
  , userURL
) where

import           Data.List (foldl')
import           Data.Maybe (fromJust)
import           Network.URL (URL)
import qualified Network.URL as URL
import           Sound.Freesound.Version2.Sound
import           System.FilePath

addPath :: String -> URL -> URL
addPath path (URL.URL ut up ups) = URL.URL ut (up </> path) ups

addParams :: [(String, String)] -> URL -> URL
addParams = flip (foldl' (\url param -> URL.add_param url param))

type APIKey = String

apiURL :: APIKey -> URL
apiURL key = URL.URL (URL.Absolute (URL.Host (URL.HTTP False) "tabasco.upf.edu" Nothing))
                     "api"
                     [("api_key", key)]

soundSearchURL :: APIKey -> URL
soundSearchURL = addPath "sounds/search" . apiURL

soundURL :: Sound -> APIKey -> URL
soundURL s = addPath ("sounds" </> show (soundId s)) . apiURL

userURL :: String -> APIKey -> URL
userURL name = addPath ("people" </> name) . apiURL
