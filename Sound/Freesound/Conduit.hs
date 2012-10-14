{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Sound.Freesound.Conduit where

import		     Control.Failure (Failure)
import			 Control.Monad (liftM)
import 			 Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.Class as R
import qualified Control.Monad.Trans.Control as R
import qualified Control.Monad.Trans.Reader as R
import qualified Data.Aeson as J
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.ByteString as B
import qualified Data.Conduit as C
import			 Data.Monoid (mempty)
import qualified Data.Text as T
import 			 Network.URI (URI)
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Data.ByteString.Lazy as L
import 			 Sound.Freesound.Version2.Sound.Search
import 			 Sound.Freesound.Version2.Sound
import qualified Sound.Freesound.Version2.Sound.Search.Filter as F
import Sound.Freesound.URI

apiKey = T.pack "9a585b9eb66b4e84b405f50e4a8185a1"

printit :: Maybe Sounds -> IO ()
printit x = putStrLn $ "Response: " ++ show x

httpRequest :: (Failure HTTP.HttpException m, R.MonadBaseControl IO m, C.MonadResource m) => HTTP.Method -> URI -> Maybe (C.Source m B.ByteString) -> R.ReaderT HTTP.Manager m (C.ResumableSource m B.ByteString)
httpRequest _ u _ = do
	man <- R.ask
	req <- R.lift $ HTTP.parseUrl (show u)
	liftM HTTP.responseBody $ R.lift $ HTTP.http req man

--main :: IO ()
--main = HTTP.withManager $ \man -> do
--	flip R.runReaderT $ withFreesound httpRequest apiKey $ do
--		let q = include "drum" & include "bass" & exclude "loop"
--		u <- searchURI q (F.username "Dolfeus") Nothing
--		liftIO $ putStrLn $ "Request: " ++ show u
--		--simpleHttp "http://www.haskell.org/") >>= liftIO . L.putStr
--		HTTP.simpleHttp (show u) >>= liftIO . printit . J.decode
