{-# LANGUAGE OverloadedStrings #-}
module Sound.Freesound.Sound.Search.Filter (
  Numerical
, equals
, between
, lessThan
, greaterThan
, Filters
, id
, username
, created
, originalFilename
, description
, tag
, license
, isRemix
, wasRemixed
, pack
, isGeotagged
, fileType
, duration
, bitdepth
, bitrate
, samplerate
, filesize
, channels
, md5
, numDownloads
, avgRating
, numRatings
, comment
, comments
) where

import           Data.Monoid (Monoid(..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Time.Clock as Time
import qualified Data.Time.Format as Time
import           Network.HTTP.Types.QueryLike (QueryValueLike(..))
import           Prelude hiding (id)
import           Sound.Freesound.Types (FileType, SoundId, toInt)
import qualified System.Locale as Time

data License = Attribution | AttributionNoncommercial | CreativeCommons0 deriving (Eq, Show)

instance QueryValueLike License where
  toQueryValue Attribution              = toQueryValue $ BS.pack "Attribution"
  toQueryValue AttributionNoncommercial = toQueryValue $ BS.pack "Attribution Noncommercial"
  toQueryValue CreativeCommons0         = toQueryValue $ BS.pack "Creative Commons 0"

-- | Numerical constraint.
data Numerical a = Equals a | Between a a | GreaterThan a | LessThan a deriving (Eq, Show)

class NumericalQueryValue a where
  toNumericalQueryValue :: a -> ByteString

instance NumericalQueryValue Double where
  toNumericalQueryValue = BS.pack . show

instance NumericalQueryValue Integer where
  toNumericalQueryValue = BS.pack . show

instance NumericalQueryValue SoundId where
  toNumericalQueryValue = BS.pack . show . toInt

instance NumericalQueryValue Time.UTCTime where
  toNumericalQueryValue = BS.pack . Time.formatTime Time.defaultTimeLocale "%FT%H:%M:%S%QZ"

equals :: a -> Numerical a
equals = Equals

between :: Ord a => a -> a -> Numerical a
between a b
  | a <= b = Between a b
  | otherwise = Between b a

greaterThan :: a -> Numerical a
greaterThan = GreaterThan

lessThan :: a -> Numerical a
lessThan = LessThan

wrap :: ByteString -> ByteString -> ByteString
wrap a b = BS.unwords [ "[", a, "TO", b, "]" ]

instance (NumericalQueryValue a) => QueryValueLike (Numerical a) where
  toQueryValue (Equals a)      = toQueryValue $ toNumericalQueryValue a
  toQueryValue (Between a b)   = toQueryValue $ wrap (toNumericalQueryValue a) (toNumericalQueryValue b)
  toQueryValue (GreaterThan a) = toQueryValue $ wrap (toNumericalQueryValue a) "*"
  toQueryValue (LessThan a)    = toQueryValue $ wrap "*" (toNumericalQueryValue a)

data Filter =
	  F_id (Numerical SoundId)	-- integer, sound id on freesound
  | F_username Text	-- string, not tokenized
  | F_created Text	-- date
  | F_original_filename Text --	string, tokenized
  | F_description Text -- string, tokenized
  | F_tag Text
  | F_license License
  | F_is_remix Bool
  | F_was_remixed Bool
  | F_pack Text
	--pack_tokenized:	string, tokenized
  | F_is_geotagged Bool
  | F_type FileType
  | F_duration (Numerical Double) -- duration of sound in seconds
  | F_bitdepth (Numerical Integer) -- WARNING is not to be trusted right now
  | F_bitrate (Numerical Integer) -- WARNING is not to be trusted right now
  | F_samplerate (Numerical Integer)
  | F_filesize (Numerical Integer) -- file size in bytes
  | F_channels (Numerical Integer) -- number of channels in sound, mostly 1 or 2, sometimes more
  | F_md5 Text -- 32-byte md5 hash of file
  | F_num_downloads (Numerical Integer) -- all zero right now (not imported data)
  | F_avg_rating (Numerical Double) -- average rating, from 0 to 5
  | F_num_ratings (Numerical Integer) -- number of ratings
  | F_comment Text -- tokenized
  | F_comments (Numerical Integer) -- number of comments
  deriving (Eq, Show)

mkF :: QueryValueLike a => ByteString -> a -> Maybe ByteString
mkF t = fmap (BS.append (BS.append t ":")) . toQueryValue

instance QueryValueLike Filter where
  toQueryValue filterSpec =
    case filterSpec of
      F_id x -> mkF "id" x
      F_username x -> mkF "username" x
      F_created x -> mkF "created" x
      F_original_filename x -> mkF "original_filename" x
      F_description x -> mkF "description" x
      F_tag x -> mkF "tag" x
      F_license x -> mkF "license" x
      F_is_remix x -> mkF "is_remix" x
      F_was_remixed x -> mkF "was_remixed" x
      F_pack x -> mkF "pack" x
      --F_pack_tokenized: string, tokenized
      F_is_geotagged x -> mkF "is_geotagged" x
      F_type x -> mkF "type" x
      F_duration x -> mkF "duration" x
      F_bitdepth x -> mkF "bitdepth" x
      F_bitrate x -> mkF "bitrate" x
      F_samplerate x -> mkF "samplerate" x
      F_filesize x -> mkF "filesize" x
      F_channels x -> mkF "channels" x
      F_md5 x -> mkF "md5" x
      F_num_downloads x -> mkF "num_downloads" x
      F_avg_rating x -> mkF "avg_rating" x
      F_num_ratings x -> mkF "num_ratings" x
      F_comment x -> mkF "comment" x
      F_comments x -> mkF "comments" x

newtype Filters = Filters [Filter] deriving (Show)

instance Monoid Filters where
  mempty = Filters []
  mappend (Filters a) (Filters b) = Filters (a++b)

instance QueryValueLike Filters where
  toQueryValue (Filters [])  = Nothing
  toQueryValue (Filters fs) = Just $ BS.unwords $ mapMaybe toQueryValue fs

fromFilter :: Filter -> Filters
fromFilter = Filters . (:[])

-- | Sound id on Freesound.
id :: Numerical SoundId -> Filters
id = fromFilter . F_id

username :: Text -> Filters
username = fromFilter . F_username

created :: Text -> Filters
created = fromFilter . F_created

originalFilename :: Text -> Filters
originalFilename = fromFilter . F_original_filename

description :: Text -> Filters
description = fromFilter . F_description

tag :: Text -> Filters
tag = fromFilter . F_tag

license :: License -> Filters
license = fromFilter . F_license

isRemix :: Bool -> Filters
isRemix = fromFilter . F_is_remix

wasRemixed :: Bool -> Filters
wasRemixed = fromFilter . F_was_remixed

pack :: Text -> Filters
pack = fromFilter . F_pack

isGeotagged :: Bool -> Filters
isGeotagged = fromFilter . F_is_geotagged

fileType :: FileType -> Filters
fileType = fromFilter . F_type

-- | Duration of sound in seconds.
duration :: Numerical Double -> Filters
duration = fromFilter . F_duration

-- | WARNING is not to be trusted right now.
bitdepth :: Numerical Integer -> Filters
bitdepth = fromFilter . F_bitdepth

-- | WARNING is not to be trusted right now.
bitrate :: Numerical Integer -> Filters
bitrate = fromFilter . F_bitrate

samplerate :: Numerical Integer -> Filters
samplerate = fromFilter . F_samplerate

-- | File size in bytes.
filesize :: Numerical Integer -> Filters
filesize = fromFilter . F_filesize

-- | Number of channels in sound, mostly 1 or 2, sometimes more.
channels :: Numerical Integer -> Filters
channels = fromFilter . F_channels

-- | 32-byte md5 hash of file.
md5 :: Text -> Filters
md5 = fromFilter . F_md5

-- | All zero right now (not imported data).
numDownloads :: Numerical Integer -> Filters
numDownloads = fromFilter . F_num_downloads

-- | Average rating, from 0 to 5.
avgRating :: Numerical Double -> Filters
avgRating = fromFilter . F_avg_rating

-- | Number of ratings.
numRatings :: Numerical Integer -> Filters
numRatings = fromFilter . F_num_ratings

comment :: Text -> Filters
comment = fromFilter . F_comment

-- | Number of comments.
comments :: Numerical Integer -> Filters
comments = fromFilter . F_comments
