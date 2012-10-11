module Sound.Freesound.Conduit where

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L

main = simpleHttp "http://www.haskell.org/" >>= L.putStr
