{-# LANGUAGE OverloadedStrings #-}
module Sound.Freesound.Version2.Sound.Search.Filter (
	Filter(..)
) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Prelude hiding (id)
import           Sound.Freesound.URI (ToQueryString(..))
import           Sound.Freesound.Version2.Sound (FileType, SoundId)

data License = Attribution | AttributionNoncommercial | CreativeCommons0 deriving (Eq, Show)

-- | Numerical constraint.
data Numerical a = EqualTo a | Between a a | GreaterThan a | LessThan a deriving (Eq, Show)

equalTo :: a -> Numerical a
equalTo = EqualTo

wrap a b = T.unwords [ "[", a, "TO", b, "]" ]

instance (Ord a, Show a) => ToQueryString (Numerical a) where
  toQueryString (EqualTo a) = T.pack (show a)
  toQueryString (Between a b)
    | a <= b    = wrap (T.pack (show a)) (T.pack (show b))
    | otherwise = wrap (T.pack (show b)) (T.pack (show a))
  toQueryString (GreaterThan a) = wrap (T.pack (show a)) "*"
  toQueryString (LessThan a) = wrap "*" (T.pack (show a))

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
  | F_duration Double -- duration of sound in seconds
  | F_bitdepth Integer -- WARNING is not to be trusted right now
  | F_bitrate (Numerical Int) -- WARNING is not to be trusted right now
  | F_samplerate (Numerical Integer)
  | F_filesize (Numerical Integer) -- file size in bytes
  | F_channels (Numerical Integer) -- number of channels in sound, mostly 1 or 2, sometimes more
  | F_md5 Text -- 32-byte md5 hash of file
  | F_num_downloads (Numerical Integer) -- all zero right now (not imported data)
  | F_avg_rating (Numerical Double) -- average rating, from 0 to 5
  | F_num_ratings (Numerical Integer) -- number of ratings
  | F_comment Text -- tokenized
  | F_comments (Numerical Integer) -- number of comments

--instance ToQueryString Filter where
--  toQueryString filterSpec =
--    case filterSpec of
--      F_id -> 