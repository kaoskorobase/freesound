{-# LANGUAGE CPP, OverloadedStrings #-}
module Sound.Freesound.Sound.Type where

import           Control.Monad (mzero)
import           Data.Aeson (FromJSON(..), Value(..), (.:), (.:?), decodeStrict)
import qualified Data.ByteString.Char8 as BS
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Network.HTTP.Types.QueryLike (QueryValueLike(..))
import           Prelude hiding (id)
import           Sound.Freesound.API (Resource, URI)
import           Sound.Freesound.List (List)
import           Sound.Freesound.Pack.Type (Pack)
import           Sound.Freesound.Time
-- import qualified Sound.Freesound.User.Type as User

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif

newtype SoundId = SoundId { soundIdToInteger :: Integer }
                  deriving (Eq, Ord, Show)

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
  -- | The name user gave to the sound.
  name :: a -> Text
  -- | An array of tags the user gave to the sound.
  tags :: a -> [Text]
  -- | The username of the uploader of the sound.
  username :: a -> Text
  -- | The license under which the sound is available to you.
  license :: a -> License

data License =
    Attribution
  | AttributionNoncommercial
  | CreativeCommons0
  | License Text
  deriving (Eq, Show)

instance FromJSON License where
  parseJSON (String x) =
    case x of
      "Attribution" -> pure Attribution
      "Attribution Noncommercial" -> pure AttributionNoncommercial
      "Creative Commons 0" -> pure CreativeCommons0
      u -> pure $ License u
  parseJSON _ = fail "Invalid license"

data Previews = Previews {
    preview_hq_mp3 :: URI -- ^ The URI for retrieving a high quality (~128kbps) mp3 preview of the sound.
  , preview_lq_mp3 :: URI -- ^ The URI for retrieving a low quality (~64kbps) mp3 preview of the sound.
  , preview_hq_ogg :: URI -- ^ The URI for retrieving a high quality (~192kbps) ogg preview of the sound.
  , preview_lq_ogg :: URI -- ^ The URI for retrieving a low quality (~80kbps) ogg of the sound.
  } deriving (Eq, Show)

instance FromJSON Previews where
  parseJSON (Object v) =
    Previews
      <$> v .: "preview-hq-mp3"
      <*> v .: "preview-lq-mp3"
      <*> v .: "preview-hq-ogg"
      <*> v .: "preview-lq-ogg"
  parseJSON _ = fail "Couldn't parse Previews"

data Images = Images {
    waveform_m :: URI -- ^ A visualization of the sounds waveform, png file (medium).
  , waveform_l :: URI -- ^ A visualization of the sounds waveform, png file (large).
  , spectral_m :: URI -- ^ A visualization of the sounds spectrum over time, jpeg file (medium).
  , spectral_l :: URI -- ^ A visualization of the sounds spectrum over time, jpeg file (large).
  } deriving (Eq, Show)

instance FromJSON Images where
  parseJSON (Object v) =
    Images
      <$> v .: "waveform_m"
      <*> v .: "waveform_l"
      <*> v .: "spectral_m"
      <*> v .: "spectral_l"
  parseJSON _ = fail "Couldn't parse Images"

data Detail = Detail {
    sound_id            :: SoundId      -- ^ The sound’s unique identifier.
  , url                 :: URI          -- ^ The URI for this sound on the Freesound website.
  , sound_name          :: Text         -- ^ The name user gave to the sound.
  , sound_tags          :: [Text]       -- ^ An array of tags the user gave to the sound.
  , description         :: Text         -- ^ The description the user gave to the sound.
  , geotag              :: Maybe GeoTag -- ^ Latitude and longitude of the geotag (only for sounds that have been geotagged).
  , created             :: UTCTime      -- ^ The date of when the sound was uploaded.
  , sound_license       :: License      -- ^ The license under which the sound is available to you.
  , fileType            :: FileType     -- ^ The type of sound (wav, aif, mp3, etc.).
  , channels            :: Int          -- ^ The number of channels.
  , filesize            :: Integer      -- ^ The size of the file in bytes.
  , bitrate             :: Int          -- ^ The bit rate of the sound.
  , bitdepth            :: Int          -- ^ The bit depth of the sound.
  , duration            :: Double       -- ^ The duration of the sound in seconds.
  , samplerate          :: Int          -- ^ The samplerate of the sound.
  , sound_username      :: Text         -- ^ The username of the uploader of the sound.
  , pack                :: Maybe (Resource Pack) -- ^ If the sound is part of a pack, this URI points to that pack’s API resource.
  , download            :: URI          -- ^ The URI for retrieving the original sound.
  , bookmark            :: URI          -- ^ The URI for bookmarking the sound.
  , previews            :: Previews     -- ^ Dictionary containing the URIs for mp3 and ogg versions of the sound. The dictionary includes the fields preview-hq-mp3 and preview-lq-mp3 (for ~128kbps quality and ~64kbps quality mp3 respectively), and preview-hq-ogg and preview-lq-ogg (for ~192kbps quality and ~80kbps quality ogg respectively).
  , images              :: Images       -- ^ Dictionary including the URIs for spectrogram and waveform visualizations of the sound. The dinctionary includes the fields waveform_l and waveform_m (for large and medium waveform images respectively), and spectral_l and spectral_m (for large and medium spectrogram images respectively).
  , numDownloads        :: Integer      -- ^ The number of times the sound was downloaded.
  , avgRating           :: Double       -- ^ The average rating of the sound.
  , numRatings          :: Integer      -- ^ The number of times the sound was rated.
  , rate                :: URI          -- ^ The URI for rating the sound.
  , comments            :: Resource ()     -- ^ The URI of a paginated list of the comments of the sound.
  , numComments         :: Integer      -- ^ The number of comments.
  , comment             :: URI          -- ^ The URI to comment the sound.
  , similarSounds       :: Resource (List Summary)     -- ^ URI pointing to the similarity resource (to get a list of similar sounds).
  -- , analysis            :: Maybe
  , analysisStats       :: Resource ()     -- ^ URI pointing to the complete analysis results of the sound.
  , analysisFrames      :: Resource ()    -- ^ The URI for retrieving a JSON file with analysis information for each frame of the sound.
  } deriving (Eq, Show)

instance FromJSON Detail where
  parseJSON (Object v) =
    Detail
      <$> v .: "id"
      <*> v .: "url"
      <*> v .: "name"
      <*> v .: "tags"
      <*> v .: "description"
      <*> v .:? "geotag"
      <*> (toUTCTime <$> (v .: "created"))
      <*> v .: "license"
      <*> v .: "type"
      <*> v .: "channels"
      <*> v .: "filesize"
      <*> v .: "bitrate"
      <*> v .: "bitdepth"
      <*> v .: "duration"
      <*> v .: "samplerate"
      <*> v .: "username"
      <*> v .:? "pack"
      <*> v .: "download"
      <*> v .: "bookmark"
      <*> v .: "previews"
      <*> v .: "images"
      <*> v .: "num_downloads"
      <*> v .: "avg_rating"
      <*> v .: "num_ratings"
      <*> v .: "rate"
      <*> v .: "comments"
      <*> v .: "num_comments"
      <*> v .: "comment"
      <*> v .: "similar_sounds"
      <*> v .: "analysis_stats"
      <*> v .: "analysis_frames"
  parseJSON _ = fail "Couldn't parse Sound"

instance Sound Detail where
  id = sound_id
  name = sound_name
  tags = sound_tags
  username = sound_username
  license = sound_license

data Summary = Summary {
    summary_id :: SoundId
  , summary_name :: Text
  , summary_tags :: [Text]
  , summary_username :: Text
  , summary_license :: License
  } deriving (Eq, Show)

instance FromJSON Summary where
  parseJSON (Object v) =
    Summary
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "tags"
      <*> v .: "username"
      <*> v .: "license"
  parseJSON _ = fail "Couldn't parse Sound"

instance Sound Summary where
  id = summary_id
  name = summary_name
  tags = summary_tags
  username = summary_username
  license = summary_license

-- | Coordinate
data GeoTag = GeoTag {
  latitude :: Double
, longitude :: Double
} deriving (Eq, Show)

instance FromJSON GeoTag where
  parseJSON (String s) = do
    let coords = T.concat ["[", T.intercalate "," (T.splitOn " " s), "]"]
    case decodeStrict (T.encodeUtf8 coords) of
      Just [lat, lon] -> return $ GeoTag lat lon
      _ -> fail "Couldn't parse GeoTag"
  parseJSON _ = fail "Couldn't parse GeoTag"
