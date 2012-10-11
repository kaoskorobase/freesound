module Sound.Freesound.Version2.Sound (
    SoundId
  , toInt
  , fromInt
  , FileType(..)
  , Tag
  , Info(..)
  , Sound(..)
) where

import           Data.Int (Int64)
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import           Network.URI (URI)
import           Prelude hiding (id)
import           Sound.Freesound.Util
import qualified Sound.Freesound.Version2.User as User

newtype SoundId = SoundId { toInt :: Int64 } deriving (Eq, Ord, Show)

fromInt :: Int64 -> SoundId
fromInt = SoundId

data FileType = WAV | AIFF | OGG | MP3 | FLAC deriving (Eq, Show)

-- | Tag (a single word) for describing a 'Sound'
type Tag = Text

data Info = Info {
    id                  :: SoundId      -- | The sound’s unique identifier.
  , ref                 :: URI          -- | The URI for this sound.
  , url                 :: URI          -- | The URI for this sound on the Freesound website.
  , preview_hq_mp3      :: URI          -- | The URI for retrieving a high quality (~128kbps) mp3 preview of the sound.
  , preview_lq_mp3      :: URI          -- | The URI for retrieving a low quality (~64kbps) mp3 preview of the sound.
  , preview_hq_ogg      :: URI          -- | The URI for retrieving a high quality (~192kbps) ogg preview of the sound.
  , preview_lq_ogg      :: URI          -- | The URI for retrieving a low quality (~80kbps) ogg of the sound.
  , serve               :: URI          -- | The URI for retrieving the original sound.
  , similar             :: URI          -- | URI pointing to the similarity resource (to get a list of similar sounds).
  , fileType            :: FileType     -- | The type of sound (wav, aif, mp3, etc.).
  , duration            :: Int          -- | The duration of the sound in seconds.
  , originalFilename    :: Text         -- | The name of the sound file when it was uploaded.
  , tags                :: [Tag]        -- | An array of tags the user gave the sound.
  , pack                :: Maybe URI    -- | If the sound is part of a pack, this URI points to that pack’s API resource.
  , user                :: User.Info    -- | A dictionary with the username, url, and ref for the user that uploaded the sound.
  , spectral_m          :: URI          -- | A visualization of the sounds spectrum over time, jpeg file (medium).
  , spectral_l          :: URI          -- | A visualization of the sounds spectrum over time, jpeg file (large).
  , waveform_m          :: URI          -- | A visualization of the sounds waveform, png file (medium).
  , waveform_l          :: URI          -- | A visualization of the sounds waveform, png file (large).
  , analysisStats       :: URI          -- | URI pointing to the analysis results of the sound (see Analysis Descriptor Documentation).
  , analysisFrames      :: URI          -- | The URI for retrieving a JSON file with analysis information for each frame of the sound (see Analysis Descriptor Documentation).
  } deriving (Eq, Show)

-- In order to unify the interface of Info and Sound maybe add a typeclass.

-- | Coordinate
data Geotag = Geotag {
    latitude :: Double
  , longitude :: Double
  } deriving (Eq, Show)

-- | Properties of a 'Sample' in the database.
data Sound = Sound {
    info                :: Info

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
