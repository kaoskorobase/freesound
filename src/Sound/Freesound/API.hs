{-# LANGUAGE CPP, FlexibleInstances, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Sound.Freesound.API (
  APIKey
, apiKeyFromString
-- , HTTPRequest
, Freesound
, runFreesound
-- , request
, URI
, Resource(..)
, resourceURI
, appendQuery
, get
) where

import qualified Blaze.ByteString.Builder as Builder
import qualified Blaze.ByteString.Builder.Char8 as Builder
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Reader as R
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Network.HTTP.Types as HTTP
import qualified Network.URI as URI
import qualified Network.Wreq as HTTP
import           Network.Wreq.Session (Session)
import qualified Network.Wreq.Session as Session

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
import           Data.Monoid
#endif

-- | API key required for each call to the Freesound server.
newtype APIKey = APIKey BS.ByteString deriving (Eq, Show)

-- | Construct an API key from a String.
apiKeyFromString :: String -> APIKey
apiKeyFromString = APIKey . BS.pack

-- | Reader monad environment.
data Env = Env {
    apiKey :: APIKey
  , session :: Session
  }

-- | Freesound API monad for communication with the Freesound server.
newtype Freesound a = Freesound (R.ReaderT Env IO a)
                        deriving (Applicative, Functor, Monad, MonadIO, R.MonadReader Env)

-- instance MonadTrans Freesound where
--   lift = Freesound . lift

-- | Perform an API action and return the result.
runFreesound :: APIKey -> Freesound a -> IO a
runFreesound k (Freesound m) = Session.withAPISession (R.runReaderT m . Env k)

-- | Newtype wrapper of Network.URI.URI to avoid orphan instance.
newtype URI = URI URI.URI deriving (Eq, Show)

instance FromJSON URI where
  parseJSON (String v) =
    maybe (fail "Couldn't parse URI")
          return
          (parseURI' (T.unpack v))
  parseJSON v = typeMismatch "URI" v

-- | Cover up for Freesound sloppiness.
parseURI' :: String -> Maybe URI
parseURI' = fmap URI . URI.parseURI . URI.escapeURIString URI.isAllowedInURI

-- | Download the data referred to by a URI.
getURI :: URI -> Freesound BL.ByteString
getURI (URI u) = do
  s <- R.asks session
  APIKey k <- R.asks apiKey
  let opts = HTTP.defaults & HTTP.header "Authorization" .~ ["Token " `BS.append` k]
      u' = URI.uriToString id u ""
  r <- liftIO $ Session.getWith opts s u'
  return $ r ^. HTTP.responseBody

-- | Resource URI.
newtype Resource a = Resource URI deriving (Eq, FromJSON, Show)

-- | Append a query string to a resource URI.
appendQuery :: HTTP.QueryLike a => a -> Resource r -> Resource r
appendQuery q (Resource (URI u)) = Resource $ URI $ u { URI.uriQuery = q' }
  where q' = BS.unpack
           $ Builder.toByteString
           $ HTTP.renderQueryBuilder True
           $ HTTP.parseQuery (BS.pack (URI.uriQuery u))
              ++ HTTP.toQuery q

-- | Get the resource referred to by a URI.
get :: (FromJSON a) => Resource a -> Freesound a
get (Resource u) = do
  handle . eitherDecode <$> getURI u
  -- TODO: Proper error handling
  where handle = either (\e -> error $ "JSON decoding failed: " ++ e) id

-- | The base URI of the Freesound API.
baseURI :: Builder.Builder
baseURI = Builder.fromString "http://www.freesound.org/apiv2"

-- | Construct an API uri from path components and a query.
resourceURI :: [Text] -> HTTP.Query -> Resource a
resourceURI path query = Resource u
  where Just u = parseURI'
               . BL.unpack
               . Builder.toLazyByteString
               . mappend baseURI
               . HTTP.encodePath path
               $ query

-- | Perform an HTTP request given the method, a URI and an optional body and return the response body as a Conduit source.
-- request :: Monad m => HTTP.Method -> URI -> Maybe (C.Source m BS.ByteString) -> FreesoundT m (C.ResumableSource m BS.ByteString)
-- request method (URI uri) body = do
--   f <- FreesoundT $ R.asks httpRequest
--   lift $ f method uri body
