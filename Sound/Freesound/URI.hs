{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Sound.Freesound.URI (
	ToQueryString(..)
  , addQueryParams
  , addPath
  , Resource
  , resourceURI
  , Data
  , dataURI
  , apiURI
  , FreesoundT
  , withFreesound
) where

import			 Control.Monad (mzero)
import qualified Control.Monad.Trans.Reader as R
import 			 Control.Monad.IO.Class (MonadIO)
import 			 Data.Aeson
import qualified Data.List as L
import			 Data.Maybe (fromJust)
import 			 Data.Text (Text)
import qualified Data.Text as T
import			 System.FilePath ((</>))
import 			 Network.URI (URI(..), parseURI, parseURIReference, relativeTo)
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

--instance ToQueryString Integer where
--	toQueryString = toQueryString . show

--instance ToQueryString Double where
--	toQueryString = toQueryString . show

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

-- Cover up for Freesound sloppiness.
parseURI' = URI.parseURI . URI.escapeURIString URI.isAllowedInURI

newtype Resource a = Resource { resourceURI :: URI } deriving (Eq)

instance Show (Resource a) where
	show = show . resourceURI

instance FromJSON a => FromJSON (Resource a) where
	parseJSON (String v) = maybe mzero (return . Resource) (parseURI' (T.unpack v))
	parseJSON _ = mzero

newtype Data = Data { dataURI :: URI } deriving (Eq)

instance Show Data where
	show = show . dataURI

instance FromJSON Data where
	parseJSON (String v) = maybe mzero (return . Data) (parseURI' (T.unpack v))
	parseJSON _ = mzero

type APIKey = Text

newtype FreesoundT m a = FreesoundT { unFreesoundT :: R.ReaderT APIKey m a }
							deriving (Functor, Monad, MonadIO)

withFreesound :: APIKey -> FreesoundT m a -> m a
withFreesound apiKey = flip R.runReaderT apiKey . unFreesoundT

fromURI :: Monad m => URI -> FreesoundT m URI
fromURI u = do
	apiKey <- FreesoundT $ R.ask
	return $ addQueryParams [("api_key", apiKey)] u

-- | The base URI of the Freesound API.
baseURI :: URI
baseURI = fromJust $ parseURI "http://freesound.org/api"

apiURI :: Monad m => String -> FreesoundT m URI
apiURI = fromURI . flip addPath baseURI
