module Sound.Freesound.URL (
    postFields, addParams
) where

import Data.List    (intercalate)
import Network.Curl (URLString)

-- | Construct a POST field from a key/value pair.
postField :: String -> String -> String
postField k v = k ++ "=" ++ v

postFields :: [(String, String)] -> [String]
postFields = map (uncurry postField)

addParams :: [(String, String)] -> URLString -> URLString
addParams [] u = u
addParams (p:ps) u = addpn ps (addp1 p u)
    where
        addp1 (k, v) u        = u ++ "?" ++ (postField k v)
        addpn []            u = u
        addpn ((k, v):rest) u = u ++ "&" ++ postField k v ++ addpn rest u
