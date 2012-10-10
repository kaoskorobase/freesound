module Sound.Freesound.Version2.Sound.Filter (
	Filter(..)
) where

import Prelude hiding (id)

data Filter = Filter
--	Filter_id Int -- integer, sound id on freesound
--username:	-- string, not tokenized
--created:	-- date
--original_filename:	string, tokenized
--description:	string, tokenized
--tag:	string
--license:	string (“Attribution”, “Attribution Noncommercial” or “Creative Commons 0”)
--is_remix:	boolean
--was_remixed:	boolean
--pack:	string
--pack_tokenized:	string, tokenized
--is_geotagged:	boolean
--type:	string, original file type, one of wav, aiff, ogg, mp3, flac
--duration:	numerical, duration of sound in seconds
--bitdepth:	integer, WARNING is not to be trusted right now
--bitrate:	numerical, WARNING is not to be trusted right now
--samplerate:	integer
--filesize:	integer, file size in bytes
--channels:	integer, number of channels in sound, mostly 1 or 2, sometimes more
--md5:	string, 32-byte md5 hash of file
--num_downloads:	integer, all zero right now (not imported data)
--avg_rating:	numerical, average rating, from 0 to 5
--num_ratings:	integer, number of ratings
--comment:	string, tokenized
--comments:	numerical, number of comments