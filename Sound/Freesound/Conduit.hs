{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Sound.Freesound.Conduit where

import           Control.Failure (Failure)
import           Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.Class as R
import qualified Control.Monad.Trans.Control as R
import qualified Control.Monad.Trans.Reader as R
import qualified Data.Aeson as J
import qualified Data.ByteString as B
import qualified Data.Conduit as C
import           Data.Monoid (mempty)
import qualified Data.Text as T
import           Network.URI (URI)
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Data.ByteString.Lazy as L
import           Sound.Freesound.Sound.Search
import           Sound.Freesound.Sound
import qualified Sound.Freesound.Sound.Search.Filter as F
import           Sound.Freesound.URI

apiKey = T.pack "9a585b9eb66b4e84b405f50e4a8185a1"

printit :: Maybe Sounds -> IO ()
printit x = putStrLn $ "Response: " ++ show x

httpRequest :: (Failure HTTP.HttpException m, R.MonadBaseControl IO m, C.MonadResource m) =>
    HTTPRequest (R.ReaderT HTTP.Manager m)
httpRequest _ u _ = do
    liftIO $ putStrLn $ "Request: " ++ show u
    man <- R.ask
    req <- HTTP.parseUrl (show u)
    res <- HTTP.http req man
    return $ HTTP.responseBody res

withManager a = HTTP.withManager $ \man -> R.runReaderT a man
withFreesound' = withManager . withFreesound httpRequest apiKey

main :: IO ()
main = withFreesound' $ do
    let q = include "drum" & include "bass" & exclude "loop"
        f = mempty -- F.username "Dolfeus"
        s = Nothing
    --simpleHttp "http://www.haskell.org/") >>= liftIO . L.putStr
    --HTTP.simpleHttp (show u) >>= liftIO . printit . J.decode
    u <- searchURI q f s
    liftIO $ putStrLn $ "Request: " ++ show u
    r <- search q f s
    liftIO $ print r
