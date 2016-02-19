module Sound.Freesound.Time (
    Time
  , toUTCTime
  , UTCTime
) where

import           Data.Aeson
import qualified Data.Text as T
import           Data.Time (UTCTime)

newtype Time = Time { toUTCTime :: UTCTime } deriving (Eq, Show)

instance FromJSON Time where
  parseJSON (String s) = Time `fmap` parseJSON (String (T.snoc s 'Z'))
  parseJSON _ = fail "Couldn't parse time"
