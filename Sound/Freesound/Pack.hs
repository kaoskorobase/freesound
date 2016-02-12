module Sound.Freesound.Pack (
  Pack(..)
, Packs
, Summary
, Detail
, user
, getSounds
, getSounds_
) where

import Data.Default (def)
import Sound.Freesound.API (Freesound, appendQuery, getResource)
import Sound.Freesound.List (List)
import Sound.Freesound.Pack.Type
import Sound.Freesound.Search (Pagination)
import Sound.Freesound.Sound (Sounds)

type Packs = List Summary

getSounds :: (Pack a) => Pagination -> a -> Freesound Sounds
getSounds p = getResource . appendQuery p . sounds

getSounds_ :: (Pack a) => a -> Freesound Sounds
getSounds_ = getSounds def
