{-# LANGUAGE OverloadedStrings #-}
module Sound.Freesound.Conduit where

import 			 Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import qualified Data.Attoparsec.ByteString.Lazy as A
import			 Data.Monoid (mempty)
import qualified Data.Text as T
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import Sound.Freesound.Version2.Sound.Search
import Sound.Freesound.Version2.Sound
import qualified Sound.Freesound.Version2.Sound.Search.Filter as F
import Sound.Freesound.URI

apiKey = T.pack "9a585b9eb66b4e84b405f50e4a8185a1"

printit :: Maybe Sounds -> IO ()
printit x = putStrLn $ "Response: " ++ show x

main :: IO ()
main = withFreesound apiKey $ do
	let q = include "drum" & include "bass" & exclude "loop"
	u <- searchURI q (F.username "Dolfeus") Nothing
	liftIO $ putStrLn $ "Request: " ++ show u
	--simpleHttp "http://www.haskell.org/") >>= liftIO . L.putStr
	simpleHttp (show u) >>= liftIO . printit . J.decode
