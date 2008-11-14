module Sound.Freesound.Properties (
    User(..),
    Description(..),
    Statistics(..),
    Tag,
    Properties(..),
    fromXML, listFromXML
) where

import Data.Maybe               (mapMaybe)
import Network.Curl             (URLString)
import Sound.Freesound.Util
import Text.XML.Light           (unqual)
import qualified Text.XML.Light as XML

-- | User of the Freesound database.
data User = User {
    userId   :: Int,
    userName :: String
} deriving (Eq, Show)

-- | Description of a 'Sample', containing user and text.
data Description = Description User String deriving (Eq, Show)

-- | Tag (a single word) for describing a 'Sound'
type Tag = String

-- | Statistics of a 'Sample' in the database.
data Statistics = Statistics {
    downloads :: Int,
    rating    :: (Int, Int)
} deriving (Eq, Show)

-- | Properties of a 'Sample' in the database.
data Properties = Properties {
    sampleId            :: Int,
    
    user                :: User,
    date                :: String,
    originalFileName    :: String,
    statistics          :: Statistics,
    
    image               :: URLString,
    preview             :: URLString,
    colors              :: URLString,

    -- descriptors ??
    -- parent ??
    -- geotag ??
    
    extension           :: String,
    sampleRate          :: Int,
    bitRate             :: Int,
    bitDepth            :: Int,
    channels            :: Int,
    duration            :: Double,
    fileSize            :: Int,
    
    descriptions        :: [Description],
    tags                :: [Tag]
    
    -- comments ??
} deriving (Eq, Show)

-- | Read a 'User' value from an 'XML.Element' in the 'Maybe' monad.
userFromXML :: XML.Element -> Maybe User
userFromXML e = do
    _userId   <- XML.findAttr (unqual "id") e >>= readMaybe
    _userName <- XML.findChild (unqual "name") e >>= return . XML.strContent
    return (User _userId _userName)

-- | Read a rating from an 'XML.Element' in the 'Maybe' monad.
ratingFromXML :: XML.Element -> Maybe (Int, Int)
ratingFromXML e = do
    count <- findAttr "count" e >>= readMaybe
    value <- strContent e >>= readMaybe
    return (count, value)
    
-- | Read a 'Statistics' value from an 'XML.Element' in the 'Maybe' monad.
statisticsFromXML :: XML.Element -> Maybe Statistics
statisticsFromXML e = do
    _downloads <- findChild "downloads" e >>= strContent >>= readMaybe
    _rating    <- findChild "rating" e >>= ratingFromXML
    return (Statistics _downloads _rating)

-- | Read a 'Description' value from an 'XML.Element' in the 'Maybe' monad.
descriptionFromXML :: XML.Element -> Maybe Description
descriptionFromXML e = do
    _user <- userFromXML e
    _text <- findChild "text" e >>= strContent
    return (Description _user _text)

-- | Read a list of 'Description's from an 'XML.Element' in the 'Maybe' monad.
descriptionsFromXML :: XML.Element -> Maybe [Description]
descriptionsFromXML = return . mapMaybe descriptionFromXML . findChildren "description"

-- | Read a list of 'Tag's from an 'XML.Element' in the 'Maybe' monad.
tagsFromXML :: XML.Element -> Maybe [Tag]
tagsFromXML = return . map XML.strContent . findChildren "tag"

-- | Read a 'Properties' value from an 'XML.Element' in the 'Maybe' monad.
fromXML :: XML.Element -> Maybe Properties
fromXML e = do
    _sampleId           <- findAttr "id" e >>= readMaybe
    _user               <- findChild "user" e >>= userFromXML
    _date               <- findChild "date" e >>= strContent
    _originalFileName   <- findChild "originalFilename" e >>= strContent
    _statistics         <- findChild "statistics" e >>= statisticsFromXML
    _image              <- findChild "image" e >>= strContent
    _preview            <- findChild "preview" e >>= strContent
    _colors             <- findChild "colors" e >>= strContent
    _extension          <- findChild "extension" e >>= strContent
    _sampleRate         <- findChild "samplerate" e >>= strContent >>= readMaybe
    _bitRate            <- findChild "bitrate" e  >>= strContent >>= readMaybe
    _bitDepth           <- findChild "bitdepth" e >>= strContent >>= readMaybe
    _channels           <- findChild "channels" e >>= strContent >>= readMaybe
    _duration           <- findChild "duration" e >>= strContent >>= readMaybe
    _fileSize           <- findChild "filesize" e >>= strContent >>= readMaybe
    _descriptions       <- findChild "descriptions" e >>= descriptionsFromXML
    _tags               <- findChild "tags" e >>= tagsFromXML
    return $ Properties {
        sampleId            = _sampleId,

        user                = _user,
        date                = _date,
        originalFileName    = _originalFileName,
        statistics          = _statistics,

        image               = _image,
        preview             = _preview,
        colors              = _colors,

        extension           = _extension,
        sampleRate          = _sampleRate,
        bitRate             = _bitRate,
        bitDepth            = _bitDepth,
        channels            = _channels,
        duration            = _duration,
        fileSize            = _fileSize,

        descriptions        = _descriptions,
        tags                = _tags
    }

-- | Read a list of 'Properties' from an 'XML.Element'.
listFromXML :: XML.Element -> [Properties]
listFromXML = mapMaybe fromXML . XML.findChildren (unqual "sample")
