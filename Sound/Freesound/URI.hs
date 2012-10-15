{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Sound.Freesound.URI (
    ToQueryString(..)
  , addQueryParams
  , addPath
  , Resource(..)
  , resourceURI
  , getResource
  , Data
  , dataURI
  , getData
  , APIKey
  , HTTPRequest
  , FreesoundT
  , withFreesound
  , apiURI
) where

import           Control.Monad (liftM, mzero)
import qualified Control.Monad.Trans.Class as R
import qualified Control.Monad.Trans.Reader as R
import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson as J
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Conduit (Source)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.List as L
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text as T
import           System.FilePath ((</>))
import qualified Network.HTTP.Types.Method as HTTP
import           Network.URI (URI(..), parseURI, parseURIReference, relativeTo)
import qualified Network.URI as URI

class ToQueryString a where
  toQueryString :: a -> String

instance ToQueryString String where
  toQueryString = URI.escapeURIString URI.isAllowedInURI

instance ToQueryString Text where
  toQueryString = toQueryString . T.unpack

instance (ToQueryString k, ToQueryString v) => ToQueryString (k, v) where
  toQueryString (k, v) = concat [ toQueryString k, "=", toQueryString v ]

instance (ToQueryString k, ToQueryString v) => ToQueryString [(k, v)] where
  toQueryString = L.intercalate "&" . map toQueryString

instance ToQueryString Bool where
  toQueryString True  = "true"
  toQueryString False = "false"

-- | Add parameter key value pairs to the query part of a URI.
addQueryParams :: (ToQueryString k, ToQueryString v) => [(k, v)] -> URI -> URI
addQueryParams ps u =
  let qs = toQueryString ps
  in case uriQuery u of
      ""  -> u { uriQuery = "?" ++ qs }
      qs' -> u { uriQuery = qs' ++ "&" ++ qs }

-- | Add a relative path to an absolute URI.
addPath :: String -> URI -> URI
addPath p u = u { uriPath = uriPath u ++ "/" ++ p }

-- | Cover up for Freesound sloppiness.
parseURI' :: String -> Maybe URI
parseURI' = URI.parseURI . URI.escapeURIString URI.isAllowedInURI

-- | Download the data referred to by a URI.
getURI :: Monad m => URI -> FreesoundT m BL.ByteString
getURI u = do
  f <- FreesoundT $ R.asks httpRequest
  u' <- fromURI u
  s <- FreesoundT $ R.lift $ f HTTP.methodGet u' Nothing
  bs <- FreesoundT $ R.lift $ s C.$$+- CL.consume
  return $ BL.fromChunks bs

newtype Resource a = Resource { resourceURI :: URI } deriving (Eq)

instance Show (Resource a) where
  show = show . resourceURI

instance FromJSON a => FromJSON (Resource a) where
  parseJSON (String v) = maybe mzero (return . Resource) (parseURI' (T.unpack v))
  parseJSON _ = mzero

getResource :: (FromJSON a, Monad m) => Resource a -> FreesoundT m a
getResource = liftM (handle . J.decode) . getURI . resourceURI
  where handle = maybe (error "Internal error: JSON decoding failed") id

newtype Data = Data { dataURI :: URI } deriving (Eq)

instance Show Data where
  show = show . dataURI

instance FromJSON Data where
  parseJSON (String v) = maybe mzero (return . Data) (parseURI' (T.unpack v))
  parseJSON _ = mzero

getData :: Monad m => Data -> FreesoundT m BL.ByteString
getData = getURI . dataURI

type APIKey = Text

type HTTPRequest m = HTTP.Method -> URI -> Maybe (C.Source m B.ByteString) -> m (C.ResumableSource m B.ByteString)

data Env m = Env {
  httpRequest :: HTTPRequest m
, apiKey :: APIKey
}

newtype FreesoundT m a = FreesoundT { unFreesoundT :: R.ReaderT (Env m) m a }
                            deriving (Functor, Monad, MonadIO)

withFreesound :: HTTPRequest m -> APIKey -> FreesoundT m a -> m a
withFreesound mkRequest apiKey = flip R.runReaderT (Env mkRequest apiKey) . unFreesoundT

-- | Construct an API request URI from an existing URI.
fromURI :: Monad m => URI -> FreesoundT m URI
fromURI u = do
  k <- FreesoundT $ R.asks apiKey
  return $ addQueryParams [("api_key", k)] u

-- | The base URI of the Freesound API.
baseURI :: URI
baseURI = fromJust $ parseURI "http://freesound.org/api"

-- | Construct an API URI relative to the base URI.
apiURI :: Monad m => String -> FreesoundT m URI
apiURI = fromURI . flip addPath baseURI
