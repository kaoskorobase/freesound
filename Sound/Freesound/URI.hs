{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Sound.Freesound.URI (
    --ToQueryString(..)
  --, addQueryParams
  --, addPath
    Resource(..)
  , getResource
  , Data
  , dataURI
  , getData
  , APIKey
  , HTTPRequest
  , FreesoundT
  , withFreesound
  , apiURI
  , getResourceURI
) where

import qualified Blaze.ByteString.Builder as Builder
import qualified Blaze.ByteString.Builder.Char8 as Builder
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS
import           Control.Monad (liftM, mzero)
import qualified Control.Monad.Trans.Class as R
import qualified Control.Monad.Trans.Reader as R
import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson as J
import qualified Data.ByteString as B
--import qualified Data.ByteString.Lazy as BL
import           Data.Conduit (Source)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.List as L
import           Data.Maybe (fromJust)
import           Data.Monoid (mappend)
import           Data.Text (Text)
import qualified Data.Text as T
import           System.FilePath ((</>))
import           Network.HTTP.Types.QueryLike (QueryLike(..), QueryValueLike(..))
import qualified Network.HTTP.Types as HTTP
import           Network.URI (URI(..), parseURI, parseURIReference, relativeTo)
import qualified Network.URI as URI

--class ToQueryString a where
--  toQueryString :: a -> String

--instance ToQueryString String where
--  toQueryString = URI.escapeURIString URI.isAllowedInURI

--instance ToQueryString Text where
--  toQueryString = toQueryString . T.unpack

--instance (ToQueryString k, ToQueryString v) => ToQueryString (k, v) where
--  toQueryString (k, v) = concat [ toQueryString k, "=", toQueryString v ]

--instance (ToQueryString k, ToQueryString v) => ToQueryString [(k, v)] where
--  toQueryString = L.intercalate "&" . map toQueryString

instance QueryValueLike Bool where
  toQueryValue True  = toQueryValue "true"
  toQueryValue False = toQueryValue "false"

-- | Add parameter key value pairs to the query part of a URI.
--addQueryParams :: (ToQueryString k, ToQueryString v) => [(k, v)] -> URI -> URI
--addQueryParams ps u =
--  let qs = toQueryString ps
--  in case uriQuery u of
--      ""  -> u { uriQuery = "?" ++ qs }
--      qs' -> u { uriQuery = qs' ++ "&" ++ qs }

-- | Add a relative path to an absolute URI.
--addPath :: String -> URI -> URI
--addPath p u = u { uriPath = uriPath u ++ "/" ++ p }

-- | Cover up for Freesound sloppiness.
parseURI' :: String -> Maybe URI
parseURI' = URI.parseURI . URI.escapeURIString URI.isAllowedInURI

-- | Download the data referred to by a URI.
getURI :: Monad m => URI -> FreesoundT m BL.ByteString
getURI u = do
  f <- FreesoundT $ R.asks httpRequest
  s <- FreesoundT $ R.lift $ f HTTP.methodGet u Nothing
  bs <- FreesoundT $ R.lift $ s C.$$+- CL.consume
  return $ BL.fromChunks bs

getResourceURI :: (FromJSON a, Monad m) => URI -> FreesoundT m a
getResourceURI u = return . handle . J.decode =<< getURI u
  where handle = maybe (error "Internal error: JSON decoding failed") id

newtype Resource a = Resource { resourceURI :: URI } deriving (Eq)

instance Show (Resource a) where
  show = show . resourceURI

instance FromJSON a => FromJSON (Resource a) where
  parseJSON (String v) = maybe mzero (return . Resource) (parseURI' (T.unpack v))
  parseJSON _ = mzero

getResource :: (FromJSON a, Monad m) => Resource a -> FreesoundT m a
getResource r = getResourceURI =<< importURI (resourceURI r)

newtype Data = Data { dataURI :: URI } deriving (Eq)

instance Show Data where
  show = show . dataURI

instance FromJSON Data where
  parseJSON (String v) = maybe mzero (return . Data) (parseURI' (T.unpack v))
  parseJSON _ = mzero

getData :: Monad m => Data -> FreesoundT m BL.ByteString
getData d = getURI =<< importURI (dataURI d)

type APIKey = String

type HTTPRequest m = HTTP.Method -> URI -> Maybe (C.Source m B.ByteString) -> m (C.ResumableSource m B.ByteString)

data Env m = Env {
  httpRequest :: HTTPRequest m
, apiKey :: APIKey
}

newtype FreesoundT m a = FreesoundT { unFreesoundT :: R.ReaderT (Env m) m a }
                            deriving (Functor, Monad, MonadIO)

withFreesound :: HTTPRequest m -> APIKey -> FreesoundT m a -> m a
withFreesound mkRequest apiKey = flip R.runReaderT (Env mkRequest apiKey) . unFreesoundT

-- | The base URI of the Freesound API.
baseURI :: Builder.Builder
baseURI = Builder.fromString "http://freesound.org/api"

apiURI :: (Monad m) => [Text] -> HTTP.Query -> FreesoundT m URI
apiURI path = importURI
            . fromJust
            . parseURI
            . BL.unpack
            . Builder.toLazyByteString
            . mappend baseURI
            . HTTP.encodePath path

-- | Construct an API request URI from an existing URI.
importURI :: Monad m => URI -> FreesoundT m URI
importURI uri = do
  k <- FreesoundT $ R.asks apiKey
  let query = BS.unpack
            $ Builder.toByteString
            $ HTTP.renderQueryBuilder True
            $ (BS.pack "api_key", Just (BS.pack k)):(HTTP.parseQuery (BS.pack (uriQuery uri)))
  return $ uri { uriQuery = query }

-- | Construct an API URI relative to the base URI.
--apiURI :: Monad m => String -> FreesoundT m URI
--apiURI = fromURI . flip addPath baseURI
