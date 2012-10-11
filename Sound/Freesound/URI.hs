{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Sound.Freesound.URI (
	ToQueryString(..)
  , addQueryParams
  , addPath
  , apiURI
  , FreesoundT
  , withFreesound
) where

import qualified Control.Monad.Trans.Reader as R
import 			 Control.Monad.IO.Class (MonadIO)
import			 Data.Maybe (fromJust)
import 			 Data.Text (Text)
import qualified Data.Text as T
import			 System.FilePath ((</>))
import 			 Network.URI (URI(..), parseURI, parseURIReference, relativeTo)

class ToQueryString a where
	toQueryString :: a -> Text

instance ToQueryString Text where
	toQueryString = id

instance ToQueryString String where
	toQueryString = T.pack

instance (ToQueryString k, ToQueryString v) => ToQueryString (k, v) where
	toQueryString (k, v) = T.concat [ toQueryString k, "=", toQueryString v ]

instance (ToQueryString k, ToQueryString v) => ToQueryString [(k, v)] where
	toQueryString = T.intercalate "&" . map toQueryString

-- | Add parameter key value pairs to the query part of a URI.
addQueryParams :: (ToQueryString k, ToQueryString v) => [(k, v)] -> URI -> URI
addQueryParams ps u =
	let qs = T.unpack (toQueryString ps)
	in case uriQuery u of
		"" -> u { uriQuery = "?" ++ qs }
		qs' -> u { uriQuery = qs' ++ "&" ++ qs }

-- | Add a relative path to an absolute URI.
addPath :: Text -> URI -> URI
addPath p u = u { uriPath = uriPath u ++ "/" ++ T.unpack p }

type APIKey = Text

newtype FreesoundT m a = FreesoundT { unFreesoundT :: R.ReaderT APIKey m a }
							deriving (Functor, Monad, MonadIO)

withFreesound :: APIKey -> FreesoundT m a -> m a
withFreesound apiKey = flip R.runReaderT apiKey . unFreesoundT

-- | The base URI of the Freesound API.
baseURI :: URI
baseURI = fromJust $ parseURI "http://freesound.org/api"

apiURI :: Monad m => FreesoundT m URI
apiURI = do
	apiKey <- FreesoundT $ R.ask
	return $ addQueryParams [(T.pack "api_key", apiKey)] baseURI
