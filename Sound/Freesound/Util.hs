module Sound.Freesound.Util (
    readMaybe,
    findAttr,
    findChild,
    findChildren,
    strContent,
    findString
) where

import Data.List                (isPrefixOf)
import qualified Text.XML.Light as XML

-- | Find substring.
findString :: String -> String -> Maybe String
findString "" xs        = Just xs
findString a  ""        = Nothing
findString a xs
    | isPrefixOf a xs   = Just xs
findString a (_:xs)     = findString a xs

-- | Read a value from a 'String', returning 'Nothing' when the parse fails.
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
                [(a, "")] -> Just a
                _         -> Nothing

-- | Find an attribute of an 'XML.Element'.
findAttr :: String -> XML.Element -> Maybe String
findAttr = XML.findAttr . XML.unqual

-- | Find a child element of an 'XML.Element'.
findChild :: String -> XML.Element -> Maybe XML.Element
findChild = XML.findChild . XML.unqual

-- | Find all child elements of an 'XML.Element'.
findChildren :: String -> XML.Element -> [XML.Element]
findChildren = XML.findChildren . XML.unqual

-- | Return the string content of an 'XML.Element' in the 'Maybe' monad.
strContent :: XML.Element -> Maybe String
strContent = return . XML.strContent
