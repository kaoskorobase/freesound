module Sound.Freesound.Pack (
  Pack(..)
, Packs
, Summary
, Detail
, username
, getSounds
, getSounds_
) where

import Data.Default (def)
import Sound.Freesound.API (Freesound, appendQuery, getResource)
import Sound.Freesound.List (List)
import Sound.Freesound.Pack.Type
import Sound.Freesound.Search (Pagination)
import qualified Sound.Freesound.Sound as Sound

type Packs = List Summary

getSounds :: (Pack a) => Pagination -> a -> Freesound (List Sound.Summary)
getSounds p = getResource . appendQuery p . sounds

getSounds_ :: (Pack a) => a -> Freesound (List Sound.Summary)
getSounds_ = getSounds def
