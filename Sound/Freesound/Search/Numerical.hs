module Sound.Freesound.Search.Numerical (
  Numerical
, equals
, between
, lessThan
, greaterThan
) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Time.Clock as Time
import qualified Data.Time.Format as Time
import           Network.HTTP.Types.QueryLike (QueryValueLike(..))
import           Sound.Freesound.Sound.Type (SoundId, soundIdToInteger)
import qualified System.Locale as Time

-- | Numerical constraint.
data Numerical a = Equals a | Between a a | GreaterThan a | LessThan a deriving (Eq, Show)

class NumericalQueryValue a where
  toNumericalQueryValue :: a -> ByteString

instance NumericalQueryValue Double where
  toNumericalQueryValue = BS.pack . show

instance NumericalQueryValue Integer where
  toNumericalQueryValue = BS.pack . show

instance NumericalQueryValue SoundId where
  toNumericalQueryValue = BS.pack . show . soundIdToInteger

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
wrap a b = BS.unwords [ BS.pack "[", a, BS.pack "TO", b, BS.pack "]" ]

wildcard :: ByteString
wildcard = BS.pack "*"

instance (NumericalQueryValue a) => QueryValueLike (Numerical a) where
  toQueryValue (Equals a)      = toQueryValue $ toNumericalQueryValue a
  toQueryValue (Between a b)   = toQueryValue $ wrap (toNumericalQueryValue a) (toNumericalQueryValue b)
  toQueryValue (GreaterThan a) = toQueryValue $ wrap (toNumericalQueryValue a) wildcard
  toQueryValue (LessThan a)    = toQueryValue $ wrap wildcard (toNumericalQueryValue a)
