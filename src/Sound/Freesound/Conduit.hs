{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
-- | A blah blah blah ...
--
-- > httpRequest :: (Failure HTTP.HttpException m, R.MonadBaseControl IO m, C.MonadResource m) =>
-- >     HTTPRequest (R.ReaderT HTTP.Manager m)
-- > httpRequest _ u _ = do
-- >     liftIO $ putStrLn $ "Request: " ++ show u
-- >     man <- R.ask
-- >     req <- HTTP.parseUrl (show u)
-- >     res <- HTTP.http req man
-- >     return $ HTTP.responseBody res

module Sound.Freesound.Conduit where

import           Control.Failure (Failure)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Trans.Control as C
import qualified Data.Aeson as J
import qualified Data.ByteString as B
import qualified Data.Conduit as C
import           Data.Default (def)
import           Data.Monoid (mempty)
import qualified Data.Text as T
import           Network.URI (URI)
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Data.ByteString.Lazy as L
import           Sound.Freesound.Sound.Search
import qualified Sound.Freesound.Sound.Search.Filter as F
import           Sound.Freesound.Types
import           Sound.Freesound.URI

apiKey :: APIKey
apiKey = "9a585b9eb66b4e84b405f50e4a8185a1"

httpRequest :: (Failure HTTP.HttpException m, C.MonadBaseControl IO m, C.MonadResource m) =>
    (String -> IO ()) -> HTTP.Manager -> HTTPRequest m
httpRequest logUri man _ uri _ = do
    liftIO $ logUri $ show uri
    HTTP.parseUrl (show uri)
        >>= flip HTTP.http man
        >>= return . HTTP.responseBody

runFreesound ::
    ( MonadIO m
    , C.MonadUnsafeIO m
    , C.MonadThrow m
    , C.MonadBaseControl IO m
    , Failure HTTP.HttpException m ) =>
    FreesoundT (C.ResourceT m) a -> m a
runFreesound a = HTTP.withManager $ \man -> withFreesound (httpRequest man) apiKey a

main :: IO ()
main = runFreesound $ do
    let q = include "drum" & include "bass" & exclude "loop"
        f = F.username "Dolfeus"
        s = Nothing
    --simpleHttp "http://www.haskell.org/") >>= liftIO . L.putStr
    --HTTP.simpleHttp (show u) >>= liftIO . printit . J.decode
    u <- searchURI def q f s
    liftIO $ putStrLn $ "Request: " ++ show u
    r <- search def q f s
    liftIO $ print r
