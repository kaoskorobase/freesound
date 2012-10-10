module Sound.Freesound.Version2.Sound.Properties (
    Tag
  , Properties(..),
) where

import           Data.Maybe (mapMaybe)
import           Network.URL (URL)
import           Sound.Freesound.Util
import           Sound.Freesound.Version2.User
import           Sound.Freesound.Version2.Sound

-- | Tag (a single word) for describing a 'Sound'
type Tag = String

-- | Properties of a 'Sample' in the database.
data Properties = Properties {
    sound               :: Sound        -- | The sound’s unique identifier.
    
  , ref 	            :: URL          -- | The URI for this sound.
  , url                 :: URL          -- | The URI for this sound on the Freesound website.

  , preview             :: URL          -- | The URI for retrieving the mp3 preview of the sound.
  , serve               :: URL          -- | The URI for retrieving the original sound.


  , fileType            :: String       -- | The type of sound (wav, aif, mp3, etc.).
  , duration            :: Int          -- | The duration of the sound in seconds.
  , samplerate          :: Int          -- | The samplerate of the sound.
  , bitdepth            :: Int          -- | The bit depth of the sound.
  , filesize            :: Int          -- | The size of the file in bytes.
  , bitrate             :: Int          -- | The bit rate of the sound.
  , channels            :: Int          -- | The number of channels.
  , originalFilename    :: String       -- | The name of the sound file when it was uploaded.

  , description         :: String       -- | The description the user gave the sound.
  , tags                :: [Tag]        -- | An array of tags the user gave the sound.
  , license             :: String       -- | The license under which the sound is available to you.
  , created             :: String       -- | The date of when the sound was uploaded.
  , numComments         :: Int          -- | The number of comments.
  , numDownloads        :: Int          -- | The number of times the sound was downloaded.
  , numRatings          :: Int          -- | The number of times the sound was rated.
  , avgRating           :: Double       -- | The average rating of the sound.

  , pack                :: Maybe URL    -- | If the sound is part of a pack, this URI points to that pack’s API resource.
  , user                :: User         -- | A dictionary with the username, url, and ref for the user that uploaded the sound.
  , spectral_m          :: URL          -- | A visualization of the sounds spectrum over time, jpeg file (medium).
  , spectral_l          :: URL          -- | A visualization of the sounds spectrum over time, jpeg file (large).
  , waveform_m          :: URL          -- | A visualization of the sounds waveform, png file (medium).
  , waveform_l          :: URL          -- | A visualization of the sounds waveform, png file (large).
} deriving (Eq, Show)

-- -- | Read a 'User' value from an 'XML.Element' in the 'Maybe' monad.
-- userFromXML :: XML.Element -> Maybe User
-- userFromXML e = do
--     _userId   <- XML.findAttr (unqual "id") e >>= readMaybe
--     _userName <- XML.findChild (unqual "name") e >>= return . XML.strContent
--     return (User _userId _userName)
-- 
-- -- | Read a rating from an 'XML.Element' in the 'Maybe' monad.
-- ratingFromXML :: XML.Element -> Maybe (Int, Int)
-- ratingFromXML e = do
--     count <- findAttr "count" e >>= readMaybe
--     value <- strContent e >>= readMaybe
--     return (count, value)
--     
-- -- | Read a 'Statistics' value from an 'XML.Element' in the 'Maybe' monad.
-- statisticsFromXML :: XML.Element -> Maybe Statistics
-- statisticsFromXML e = do
--     _downloads <- findChild "downloads" e >>= strContent >>= readMaybe
--     _rating    <- findChild "rating" e >>= ratingFromXML
--     return (Statistics _downloads _rating)
-- 
-- -- | Read a 'Description' value from an 'XML.Element' in the 'Maybe' monad.
-- descriptionFromXML :: XML.Element -> Maybe Description
-- descriptionFromXML e = do
--     _user <- userFromXML e
--     _text <- findChild "text" e >>= strContent
--     return (Description _user _text)
-- 
-- -- | Read a list of 'Description's from an 'XML.Element' in the 'Maybe' monad.
-- descriptionsFromXML :: XML.Element -> Maybe [Description]
-- descriptionsFromXML = return . mapMaybe descriptionFromXML . findChildren "description"
-- 
-- -- | Read a list of 'Tag's from an 'XML.Element' in the 'Maybe' monad.
-- tagsFromXML :: XML.Element -> Maybe [Tag]
-- tagsFromXML = return . map XML.strContent . findChildren "tag"
-- 
-- -- | Read a 'Properties' value from an 'XML.Element' in the 'Maybe' monad.
-- fromXML :: XML.Element -> Maybe Properties
-- fromXML e = do
--     _sampleId           <- findAttr "id" e >>= readMaybe
--     _user               <- findChild "user" e >>= userFromXML
--     _date               <- findChild "date" e >>= strContent
--     _originalFileName   <- findChild "originalFilename" e >>= strContent
--     _statistics         <- findChild "statistics" e >>= statisticsFromXML
--     _image              <- findChild "image" e >>= strContent
--     _preview            <- findChild "preview" e >>= strContent
--     _colors             <- findChild "colors" e >>= strContent
--     _extension          <- findChild "extension" e >>= strContent
--     _sampleRate         <- findChild "samplerate" e >>= strContent >>= readMaybe
--     _bitRate            <- findChild "bitrate" e  >>= strContent >>= readMaybe
--     _bitDepth           <- findChild "bitdepth" e >>= strContent >>= readMaybe
--     _channels           <- findChild "channels" e >>= strContent >>= readMaybe
--     _duration           <- findChild "duration" e >>= strContent >>= readMaybe
--     _fileSize           <- findChild "filesize" e >>= strContent >>= readMaybe
--     _descriptions       <- findChild "descriptions" e >>= descriptionsFromXML
--     _tags               <- findChild "tags" e >>= tagsFromXML
--     return $ Properties {
--         sampleId            = _sampleId,
-- 
--         user                = _user,
--         date                = _date,
--         originalFileName    = _originalFileName,
--         statistics          = _statistics,
-- 
--         image               = _image,
--         preview             = _preview,
--         colors              = _colors,
-- 
--         extension           = _extension,
--         sampleRate          = _sampleRate,
--         bitRate             = _bitRate,
--         bitDepth            = _bitDepth,
--         channels            = _channels,
--         duration            = _duration,
--         fileSize            = _fileSize,
-- 
--         descriptions        = _descriptions,
--         tags                = _tags
--     }
-- 
-- -- | Read a list of 'Properties' from an 'XML.Element'.
-- listFromXML :: XML.Element -> [Properties]
-- listFromXML = mapMaybe fromXML . XML.findChildren (unqual "sample")
