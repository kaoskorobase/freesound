{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Sound.Freesound.API (
  APIKey
, HTTPRequest
, FreesoundT
, runFreesoundT
, URI
, appendQuery
, apiURI
, importURI
, request
, getURI
, getResource
) where

import qualified Blaze.ByteString.Builder as Builder
import qualified Blaze.ByteString.Builder.Char8 as Builder
import           Control.Monad (mzero)
import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Reader as R
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Aeson as J
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Data.Monoid (mappend)
import           Data.Text (Text)
import qualified Data.Text as T
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Class (MonadTrans(..))
--import           Network.HTTP.Types.QueryLike (QueryValueLike(..))
import qualified Network.HTTP.Types as HTTP
import qualified Network.URI as URI

-- | API key required for each call to the Freesound server.
type APIKey = String

-- | HTTP request function.
type HTTPRequest m =
     HTTP.Method
  -> URI
  -> Maybe (C.Source m BS.ByteString)
  -> m (C.ResumableSource m BS.ByteString)

data Env m = Env {
    httpRequest :: HTTPRequest m
  , apiKey :: APIKey
  }

-- | Freesound API monad transformer for communication with the Freesound server.
newtype FreesoundT m a = FreesoundT { unFreesoundT :: R.ReaderT (Env m) m a }
                            deriving (Functor, Monad, MonadIO)

instance MonadTrans FreesoundT where
  lift = FreesoundT . lift

-- | Perform an API action and return the result.
runFreesoundT :: HTTPRequest m -> APIKey -> FreesoundT m a -> m a
runFreesoundT f k = flip R.runReaderT (Env f k) . unFreesoundT

--instance QueryValueLike Bool where
--  toQueryValue True  = toQueryValue "true"
--  toQueryValue False = toQueryValue "false"

newtype URI = URI URI.URI deriving (Eq, Show)

instance FromJSON URI where
  parseJSON (String v) = maybe mzero return (parseURI' (T.unpack v))
  parseJSON _ = mzero

-- | Cover up for Freesound sloppiness.
parseURI' :: String -> Maybe URI
parseURI' = fmap URI . URI.parseURI . URI.escapeURIString URI.isAllowedInURI

-- | Append a query string to a URI.
appendQuery :: HTTP.Query -> URI -> URI
appendQuery q (URI u) = URI (u { URI.uriQuery = q' })
  where q' = BS.unpack
           $ Builder.toByteString
           $ HTTP.renderQueryBuilder True
           $ HTTP.parseQuery (BS.pack (URI.uriQuery u)) ++ q

-- | Download the data referred to by a URI.
getURI :: Monad m => URI -> FreesoundT m BL.ByteString
getURI u = do
  s  <- request HTTP.methodGet u Nothing
  bs <- lift $ s C.$$+- CL.consume
  return $ BL.fromChunks bs

-- | Download the JSON resource referred to by a URI.
getResource :: (FromJSON a, Monad m) => URI -> FreesoundT m a
getResource u = return . handle . J.decode =<< getURI u
  where handle = maybe (error "Internal error: JSON decoding failed") id

apiKeyQuery :: Monad m => FreesoundT m HTTP.Query
apiKeyQuery = do
  k <- FreesoundT $ R.asks apiKey
  return $ [(BS.pack "api_key", Just (BS.pack k))]

--addKeyToQuery :: Monad m => HTTP.Query -> FreesoundT m HTTP.Query
--addKeyToQuery q = do
--  k <- FreesoundT $ R.asks apiKey
--  return $ (BS.pack "api_key", Just (BS.pack k)) : q

-- | The base URI of the Freesound API.
baseURI :: Builder.Builder
baseURI = Builder.fromString "http://freesound.org/api"

-- | Construct an API uri from path components and a query.
apiURI :: (Monad m) => [Text] -> HTTP.Query -> FreesoundT m URI
apiURI path query = do
  ak <- apiKeyQuery
  let Just u = parseURI'
             . BL.unpack
             . Builder.toLazyByteString
             . mappend baseURI
             . HTTP.encodePath path
             $ ak ++ query
  return u

-- | Construct an API request URI from an existing URI.
importURI :: Monad m => URI -> FreesoundT m URI
importURI uri = apiKeyQuery >>= return . flip appendQuery uri

-- | Perform an HTTP request given the method, a URI and an optional body and return the response body as a Conduit source.
request :: Monad m => HTTP.Method -> URI -> Maybe (C.Source m BS.ByteString) -> FreesoundT m (C.ResumableSource m BS.ByteString)
request method uri body = do
  f <- FreesoundT $ R.asks httpRequest
  lift $ f method uri body
