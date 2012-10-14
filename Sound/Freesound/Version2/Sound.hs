{-# LANGUAGE OverloadedStrings #-}
module Sound.Freesound.Version2.Sound (
    SoundId
  , toInt
  --, fromInt
  , FileType(..)
  , Tag
  , SoundSummary(..)
  , Sound(..)
  , Sounds(..)
  , UserSummary(..)
  , User(..)
  , Pack(..)
) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (mzero)
import           Data.Aeson
import           Data.Int (Int64)
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import           Network.URI (URI)
import           Prelude hiding (id)
import           Sound.Freesound.URI
--import qualified Sound.Freesound.Version2.User as User

newtype SoundId = SoundId { toInt :: Integer }  deriving (Eq, Ord, Show)

instance FromJSON SoundId where
  parseJSON (Number i) = return $ SoundId (truncate i)
  parseJSON _ = mzero

--fromInt :: Int64 -> SoundId
--fromInt = SoundId

data FileType = WAV | AIFF | OGG | MP3 | FLAC deriving (Eq, Show)

instance ToQueryString FileType where
  toQueryString WAV  = "wav"
  toQueryString AIFF = "aif"
  toQueryString OGG  = "ogg"
  toQueryString MP3  = "mp3"
  toQueryString FLAC = "flac"

instance FromJSON FileType where
  parseJSON (String v) =
    case v of
      "wav" -> return WAV
      "aif" -> return AIFF -- FIXME: this is aiff in the Freesound docs
      "ogg" -> return OGG
      "mp3" -> return MP3
      "flac" -> return FLAC
      _ -> mzero
  parseJSON _  = mzero

-- | Tag (a single word) for describing a 'Sound'
type Tag = Text

data SoundSummary = SoundSummary {
    id                  :: SoundId                -- | The sound’s unique identifier.
  , ref                 :: Resource Sound         -- | The URI for this sound.
  , url                 :: Data                   -- | The URI for this sound on the Freesound website.
  , preview_hq_mp3      :: Data                   -- | The URI for retrieving a high quality (~128kbps) mp3 preview of the sound.
  , preview_lq_mp3      :: Data                   -- | The URI for retrieving a low quality (~64kbps) mp3 preview of the sound.
  , preview_hq_ogg      :: Data                   -- | The URI for retrieving a high quality (~192kbps) ogg preview of the sound.
  , preview_lq_ogg      :: Data                   -- | The URI for retrieving a low quality (~80kbps) ogg of the sound.
  , serve               :: Data                   -- | The URI for retrieving the original sound.
  , similarity          :: Resource Sounds        -- | URI pointing to the similarity resource (to get a list of similar sounds).
  , fileType            :: FileType               -- | The type of sound (wav, aif, mp3, etc.).
  , duration            :: Double                 -- | The duration of the sound in seconds.
  , originalFilename    :: Text                   -- | The name of the sound file when it was uploaded.
  , tags                :: [Tag]                  -- | An array of tags the user gave the sound.
  , pack                :: Maybe (Resource Pack)  -- | If the sound is part of a pack, this URI points to that pack’s API resource.
  , user                :: UserSummary            -- | A dictionary with the username, url, and ref for the user that uploaded the sound.
  , spectral_m          :: Data                   -- | A visualization of the sounds spectrum over time, jpeg file (medium).
  , spectral_l          :: Data                   -- | A visualization of the sounds spectrum over time, jpeg file (large).
  , waveform_m          :: Data                   -- | A visualization of the sounds waveform, png file (medium).
  , waveform_l          :: Data                   -- | A visualization of the sounds waveform, png file (large).
  , analysisStats       :: Data                   -- | URI pointing to the analysis results of the sound (see Analysis Descriptor Documentation).
  , analysisFrames      :: Data                   -- | The URI for retrieving a JSON file with analysis information for each frame of the sound (see Analysis Descriptor Documentation).
  } deriving (Eq, Show)

instance FromJSON SoundSummary where
  parseJSON (Object v) =
    SoundSummary
    <$> v .: "id"
    <*> v .: "ref"
    <*> v .: "url"
    <*> v .: "preview-hq-mp3"
    <*> v .: "preview-lq-mp3"
    <*> v .: "preview-hq-ogg"
    <*> v .: "preview-lq-ogg"
    <*> v .: "serve"
    <*> v .: "similarity"
    <*> v .: "type"
    <*> v .: "duration"
    <*> v .: "original_filename"
    <*> v .: "tags"
    <*> v .:? "pack"
    <*> v .: "user"
    <*> v .: "spectral_m"
    <*> v .: "spectral_l"
    <*> v .: "waveform_m"
    <*> v .: "waveform_l"
    <*> v .: "analysis_stats"
    <*> v .: "analysis_frames"
  parseJSON _ = mzero

-- In order to unify the interface of Info and Sound maybe add a typeclass.

-- | Coordinate
data Geotag = Geotag {
  latitude :: Double
, longitude :: Double
} deriving (Eq, Show)

instance FromJSON Geotag where
  parseJSON (Object v) =
    Geotag
    <$> v .: "lat"
    <*> v .: "lon"
  parseJSON _ = mzero

-- | Properties of a 'Sample' in the database.
data Sound = Sound {
  sound_summary       :: SoundSummary

, samplerate          :: Int          -- | The samplerate of the sound.
, bitdepth            :: Int          -- | The bit depth of the sound.
, filesize            :: Int64        -- | The size of the file in bytes.
, bitrate             :: Int          -- | The bit rate of the sound.
, channels            :: Int          -- | The number of channels.

, description         :: Text         -- | The description the user gave the sound.
, license             :: Text         -- | The license under which the sound is available to you.
, created             :: Text         -- | The date of when the sound was uploaded.
, numComments         :: Int          -- | The number of comments.
, numDownloads        :: Int          -- | The number of times the sound was downloaded.
, numRatings          :: Int          -- | The number of times the sound was rated.
, avgRating           :: Double       -- | The average rating of the sound.

, geotag              :: Maybe Geotag -- | A dictionary with the latitude (‘lat’) and longitude (‘lon’) of the geotag (only for sounds that have been geotagged).
} deriving (Eq, Show)

instance FromJSON Sound where
  parseJSON j@(Object v) =
    Sound
    <$> parseJSON j
    <*> v .: "samplerate"
    <*> v .: "bitdepth"
    <*> v .: "filesize"
    <*> v .: "bitrate"
    <*> v .: "channels"
    <*> v .: "description"
    <*> v .: "license"
    <*> v .: "created"
    <*> v .: "num_comments"
    <*> v .: "num_downloads"
    <*> v .: "num_ratings"
    <*> v .: "avg_rating"
    <*> v .: "geotag"
  parseJSON _ = mzero

data Sounds = Sounds {
  sounds    :: [SoundSummary]
, numSounds :: Int
, numPages  :: Int
, previous  :: Maybe (Resource Sounds)
, next      :: Maybe (Resource Sounds)
} deriving (Eq, Show)

instance FromJSON Sounds where
  parseJSON (Object v) = Sounds
                          <$> v .: "sounds"
                          <*> v .: "num_results"
                          <*> v .: "num_pages"
                          <*> v .:? "previous"
                          <*> v .:? "next"
  parseJSON _ = mzero

-- | User of the Freesound database.
data UserSummary = UserSummary {
  username :: Text            -- | The user’s username.
, user_ref  :: Resource User  -- | The URI for this resource.
, user_url :: Data            -- | The profile page for the user on the Freesound website.
} deriving (Eq, Show)

instance FromJSON UserSummary where
  parseJSON (Object v) =
    UserSummary
    <$> v .: "username"
    <*> v .: "ref"
    <*> v .: "url"
  parseJSON _ = mzero

data User = User {
  user_summary :: UserSummary -- | Summary.
, user_sounds :: Resource Sounds   -- | The API URI for this user’s sound collection.
, packs :: Data               -- | The API URI for this user’s pack collection.
, first_name :: Text          -- | The user’s first name, possibly empty.
, last_name :: Text           -- | The user’s last name, possibly empty.
, about :: Text               -- | A small text the user wrote about himself.
, home_page :: Maybe Data     -- | The user’s homepage, possibly empty.
, signature :: Text           -- | The user’s signature, possibly empty.
, dateJoined :: Text          -- | The date the user joined Freesound.
} deriving (Eq, Show)

instance FromJSON User where
  parseJSON j@(Object v) =
    User
    <$> parseJSON j
    <*> v .: "sounds"
    <*> v .: "packs"
    <*> v .: "first_name"
    <*> v .: "last_name"
    <*> v .: "about"
    <*> v .: "home_page"
    <*> v .: "signature"
    <*> v .: "date_joined"
  parseJSON _ = mzero

data Pack = Pack {
  pack_ref :: Resource Pack         -- | The URI for this resource.
, pack_url :: Data                  -- | The URL for this pack’s page on the Freesound website.
, pack_sounds :: Resource Sounds    -- | The API URI for the pack’s sound collection.
, pack_user :: Resource UserSummary -- | A JSON object with the user’s username, url, and ref.
, pack_name :: Text                 -- | The pack’s name.
, pack_created :: Text              -- | The date when the pack was created.
, pack_numDownloads :: Integer      -- | The number of times the pack was downloaded. 
} deriving (Eq, Show)

instance FromJSON Pack where
  parseJSON (Object v) =
    Pack
    <$> v .: "ref"
    <*> v .: "url"
    <*> v .: "sounds"
    <*> v .: "user"
    <*> v .: "name"
    <*> v .: "created"
    <*> v .: "num_downloads"
  parseJSON _ = mzero

