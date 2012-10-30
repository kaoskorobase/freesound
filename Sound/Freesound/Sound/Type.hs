{-# LANGUAGE OverloadedStrings #-}
module Sound.Freesound.Sound.Type where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (mzero)
import           Data.Aeson (FromJSON(..), Value(..), (.:), (.:?))
import qualified Data.ByteString.Char8 as BS
import           Data.Text (Text)
import           Network.HTTP.Types.QueryLike (QueryValueLike(..))
import           Prelude hiding (id)
import           Sound.Freesound.API (Resource, URI)
import           Sound.Freesound.List (Elem(..))
import qualified Sound.Freesound.User.Type as User

newtype SoundId = SoundId Integer
                  deriving (Eq, Ord, Show)

soundIdToInteger :: SoundId -> Integer
soundIdToInteger (SoundId i) = i

instance FromJSON SoundId where
  parseJSON (Number i) = return $ SoundId (truncate i)
  parseJSON _ = mzero

--fromInt :: Int64 -> SoundId
--fromInt = SoundId

data FileType = WAV | AIFF | OGG | MP3 | FLAC
                deriving (Eq, Show)

instance QueryValueLike FileType where
  toQueryValue WAV  = toQueryValue $ BS.pack "wav"
  toQueryValue AIFF = toQueryValue $ BS.pack "aif"
  toQueryValue OGG  = toQueryValue $ BS.pack "ogg"
  toQueryValue MP3  = toQueryValue $ BS.pack "mp3"
  toQueryValue FLAC = toQueryValue $ BS.pack "flac"

instance FromJSON FileType where
  parseJSON (String v) =
    case v of
      "wav"  -> return WAV
      "aiff" -> return AIFF
      "aif"  -> return AIFF
      "ogg"  -> return OGG
      "mp3"  -> return MP3
      "flac" -> return FLAC
      _      -> mzero
  parseJSON _  = mzero

class Sound a where
  -- | The sound’s unique identifier.
  id :: a -> SoundId
  -- | The URI for this sound.
  ref :: a -> Resource
  -- | The URI for this sound on the Freesound website.
  url :: a -> URI

  -- | The URI for retrieving a high quality (~128kbps) mp3 preview of the sound.
  preview_hq_mp3 :: a -> URI
  -- | The URI for retrieving a low quality (~64kbps) mp3 preview of the sound.
  preview_lq_mp3 :: a -> URI
  -- | The URI for retrieving a high quality (~192kbps) ogg preview of the sound.
  preview_hq_ogg :: a -> URI
  -- | The URI for retrieving a low quality (~80kbps) ogg of the sound.
  preview_lq_ogg :: a -> URI

  -- | The URI for retrieving the original sound.
  serve :: a -> URI
  -- | URI pointing to the similarity resource (to get a list of similar sounds).
  similarity :: a -> Resource
  -- | The type of sound (wav, aif, mp3, etc.).
  fileType :: a -> FileType
  -- | The duration of the sound in seconds.
  duration :: a -> Double
  -- | The name of the sound file when it was uploaded.
  originalFilename :: a -> Text
  -- | An array of tags the user gave the sound.
  tags :: a -> [Text]
  -- | If the sound is part of a pack, this URI points to that pack’s API resource.
  pack :: a -> Maybe Resource
  -- | A dictionary with the username, url, and ref for the user that uploaded the sound.
  user :: a -> User.Summary

  -- | A visualization of the sounds spectrum over time, jpeg file (medium).
  spectral_m :: a -> URI
  -- | A visualization of the sounds spectrum over time, jpeg file (large).
  spectral_l :: a -> URI
  -- | A visualization of the sounds waveform, png file (medium).
  waveform_m :: a -> URI
  -- | A visualization of the sounds waveform, png file (large).
  waveform_l :: a -> URI

  -- | URI pointing to the analysis results of the sound (see Analysis Descriptor Documentation).
  analysisStats  :: a -> Resource
  -- | The URI for retrieving a JSON file with analysis information for each frame of the sound (see Analysis Descriptor Documentation).
  analysisFrames :: a -> Resource

data Summary = Summary {
    sound_id                  :: SoundId                -- ^ The sound’s unique identifier.
  , sound_ref                 :: Resource               -- ^ The URI for this sound.
  , sound_url                 :: URI                    -- ^ The URI for this sound on the Freesound website.
  , sound_preview_hq_mp3      :: URI                    -- ^ The URI for retrieving a high quality (~128kbps) mp3 preview of the sound.
  , sound_preview_lq_mp3      :: URI                    -- ^ The URI for retrieving a low quality (~64kbps) mp3 preview of the sound.
  , sound_preview_hq_ogg      :: URI                    -- ^ The URI for retrieving a high quality (~192kbps) ogg preview of the sound.
  , sound_preview_lq_ogg      :: URI                    -- ^ The URI for retrieving a low quality (~80kbps) ogg of the sound.
  , sound_serve               :: URI                    -- ^ The URI for retrieving the original sound.
  , sound_similarity          :: Resource               -- ^ URI pointing to the similarity resource (to get a list of similar sounds).
  , sound_type                :: FileType               -- ^ The type of sound (wav, aif, mp3, etc.).
  , sound_duration            :: Double                 -- ^ The duration of the sound in seconds.
  , sound_original_filename   :: Text                   -- ^ The name of the sound file when it was uploaded.
  , sound_tags                :: [Text]                 -- ^ An array of tags the user gave the sound.
  , sound_pack                :: Maybe Resource         -- ^ If the sound is part of a pack, this URI points to that pack’s API resource.
  , sound_user                :: User.Summary           -- ^ A dictionary with the username, url, and ref for the user that uploaded the sound.
  , sound_spectral_m          :: URI                    -- ^ A visualization of the sounds spectrum over time, jpeg file (medium).
  , sound_spectral_l          :: URI                    -- ^ A visualization of the sounds spectrum over time, jpeg file (large).
  , sound_waveform_m          :: URI                    -- ^ A visualization of the sounds waveform, png file (medium).
  , sound_waveform_l          :: URI                    -- ^ A visualization of the sounds waveform, png file (large).
  , sound_analysis_stats      :: Resource               -- ^ URI pointing to the analysis results of the sound (see Analysis Descriptor Documentation).
  , sound_analysis_frames     :: Resource               -- ^ The URI for retrieving a JSON file with analysis information for each frame of the sound (see Analysis Descriptor Documentation).
  } deriving (Eq, Show)

instance Sound Summary where
  id               = sound_id
  ref              = sound_ref
  url              = sound_url
  preview_hq_mp3   = sound_preview_hq_mp3
  preview_lq_mp3   = sound_preview_lq_mp3
  preview_hq_ogg   = sound_preview_hq_ogg
  preview_lq_ogg   = sound_preview_lq_ogg
  serve            = sound_serve
  similarity       = sound_similarity
  fileType         = sound_type
  duration         = sound_duration
  originalFilename = sound_original_filename
  tags             = sound_tags
  pack             = sound_pack
  user             = sound_user
  waveform_m       = sound_waveform_m
  waveform_l       = sound_waveform_l
  spectral_m       = sound_spectral_m
  spectral_l       = sound_spectral_l
  analysisStats    = sound_analysis_stats
  analysisFrames   = sound_analysis_frames

instance FromJSON Summary where
  parseJSON (Object v) =
    Summary
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

instance Elem Summary where
  elemsFieldName _ = "sounds"

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
data Detail = Detail {
  summary             :: Summary

, samplerate          :: Int          -- ^ The samplerate of the sound.
, bitdepth            :: Int          -- ^ The bit depth of the sound.
, filesize            :: Integer      -- ^ The size of the file in bytes.
, bitrate             :: Int          -- ^ The bit rate of the sound.
, channels            :: Int          -- ^ The number of channels.

, description         :: Text         -- ^ The description the user gave the sound.
, license             :: URI          -- ^ The license under which the sound is available to you.
, created             :: Text         -- ^ The date of when the sound was uploaded.
, numComments         :: Integer      -- ^ The number of comments.
, numDownloads        :: Integer      -- ^ The number of times the sound was downloaded.
, numRatings          :: Integer      -- ^ The number of times the sound was rated.
, avgRating           :: Double       -- ^ The average rating of the sound.

, geotag              :: Maybe Geotag -- ^ A dictionary with the latitude (‘lat’) and longitude (‘lon’) of the geotag (only for sounds that have been geotagged).
} deriving (Eq, Show)

instance Sound Detail where
  id               = id . summary
  ref              = ref . summary
  url              = url . summary
  preview_hq_mp3   = preview_hq_mp3 . summary
  preview_lq_mp3   = preview_lq_mp3 . summary
  preview_hq_ogg   = preview_hq_ogg . summary
  preview_lq_ogg   = preview_lq_ogg . summary
  serve            = serve . summary
  similarity       = similarity . summary
  fileType         = fileType . summary
  duration         = duration . summary
  originalFilename = originalFilename . summary
  tags             = tags . summary
  pack             = pack . summary
  user             = user . summary
  waveform_m       = waveform_m . summary
  waveform_l       = waveform_l . summary
  spectral_m       = spectral_m . summary
  spectral_l       = spectral_l . summary
  analysisStats    = analysisStats . summary
  analysisFrames   = analysisFrames . summary

instance FromJSON Detail where
  parseJSON j@(Object v) =
    Detail
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
    <*> v .:? "geotag"
  parseJSON _ = mzero

--data Sounds = Sounds {
--  sounds    :: [SoundSummary]
--, numSounds :: Int
--, numPages  :: Int
--, previous  :: Maybe (Resource Sounds)
--, next      :: Maybe (Resource Sounds)
--} deriving (Eq, Show)

--type Sounds = List Summary
