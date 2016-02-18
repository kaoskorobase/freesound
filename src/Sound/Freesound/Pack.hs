module Sound.Freesound.Pack (
    Pack
  , id
  , url
  , description
  , created
  , name
  , username
  , numSounds
  , sounds
  , numDownloads
) where

import           Prelude hiding (id)
import           Sound.Freesound.API
import           Sound.Freesound.List (List)
import           Sound.Freesound.Pack.Type hiding (sounds)
import qualified Sound.Freesound.Pack.Type as Pack
import qualified Sound.Freesound.Sound as Sound

-- ^ The URI for a list of sounds in the pack.
sounds :: Pack -> Resource (List Sound.Summary)
sounds = Resource . Pack.sounds
