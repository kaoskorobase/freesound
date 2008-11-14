module Sound.Freesound.Sample (
    Sample(..),
    fromXML, listFromXML
) where

import Data.Maybe               (mapMaybe)
import Sound.Freesound.Util
import qualified Text.XML.Light as XML

-- | A handle to a sample in the database.
newtype Sample = Sample { sampleId :: Int }
    deriving (Eq, Ord, Read, Show)

-- | Read a 'Sample' from an 'XML.Element'.
fromXML :: XML.Element -> Maybe Sample
fromXML e = Sample `fmap` (findAttr "id" e >>= readMaybe)

-- | Read a list of 'Sample's from an 'XML.Element'.
listFromXML :: XML.Element -> [Sample]
listFromXML = mapMaybe fromXML . findChildren "sample"
