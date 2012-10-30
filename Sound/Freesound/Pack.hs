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
import Sound.Freesound.API (FreesoundT, appendQuery, getResource)
import Sound.Freesound.List (List)
import Sound.Freesound.Pack.Type
import Sound.Freesound.Search (Pagination)
import Sound.Freesound.Sound (Sounds)

type Packs = List Summary

getSounds :: (Pack a, Monad m) => Pagination -> a -> FreesoundT m Sounds
getSounds p = getResource . appendQuery p . sounds

getSounds_ :: (Pack a, Monad m) => a -> FreesoundT m Sounds
getSounds_ = getSounds def
