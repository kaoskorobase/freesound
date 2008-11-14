-- | This module provides access to the Freesound Project, a database of
-- Creative Commons licensed sounds.
--
-- * <http://www.freesound.org/>
--
-- * <http://www.creativecommons.org/>
module Sound.Freesound (
    -- * The Freesound monad
    Freesound,
    withFreesound,
    -- * Error handling
    Error(..), errorString,
    -- * Sample handles
    Sample(..),
    -- * API methods
    search,
    Similarity(..), searchSimilar,
    propertiesXML, properties,
    download
) where

import Data.List                                    (find, intercalate, stripPrefix)
import Data.Maybe                                   (listToMaybe, mapMaybe)
import Network.Curl
import qualified Network.Curl.Easy                  as Curl
import Control.Monad.Error                          (ErrorT(..), MonadError, MonadIO, liftIO, noMsg, strMsg, throwError)
import Control.Monad.Reader                         (MonadReader, ReaderT(..), ask)
import qualified Control.Monad.Error                as Error
import Numeric                                      (readDec)
import qualified Text.XML.Light                     as XML

import Sound.Freesound.Properties                   (Properties)
import qualified Sound.Freesound.Properties         as Properties
import Sound.Freesound.Query                        (Query)
import qualified Sound.Freesound.Query              as Query
import Sound.Freesound.Sample                       (Sample(..))
import qualified Sound.Freesound.Sample             as Sample
import qualified Sound.Freesound.URL                as URL
import Sound.Freesound.Util                         (findString, readMaybe)

mkURL :: String -> URLString
mkURL page = baseURL ++ "/" ++ page

-- The various API URLs.
baseURL          = "http://www.freesound.org"
loginURL         = mkURL "forum/login.php"
loginRedirect    = "../index.php"
searchURL        = mkURL "searchTextXML.php"
searchSimilarURL = mkURL "searchSimilarXML.php"
xmlURL           = mkURL "samplesViewSingleXML.php"
audioURL         = mkURL "samplesDownload.php"

-- | Curl options used by default in all the interaction with the database.
defaultCurlOptions :: [CurlOption]
defaultCurlOptions = [
        -- Some servers require this to be set
        CurlUserAgent "libcurl-agent/1.0",
        -- Enable cookie handling; cookies are passed around in the session
        -- handle and not saved to disk
        CurlCookieFile ""
    ]

-- | Error type.
data Error =
      Error String
    | CurlError CurlCode
    | LoginError
    | XMLError
    | UnknownError
    deriving (Show)

instance Error.Error Error where
  noMsg    = UnknownError
  strMsg s = Error s

-- | Convert an 'Error' into a 'String'.
errorString :: Error -> String
errorString (Error s)     = s
errorString (CurlError c) = maybe s id (stripPrefix "Curl" (show c))
    where s = show c
errorString e             = show e

-- | Curl handle.
type Handle = Curl

-- | The 'Freesound' monad.

-- Adds an environment (the 'Curl' handle) and error handling to the 'IO'
-- monad. Actions in the 'Freesound' monad can only be executed by
-- 'withFreesound', which handles all the initialization and cleanup details.
newtype Freesound a = Freesound { fromFreesound :: ReaderT Handle (ErrorT Error IO) a }
                        deriving (Functor, Monad, MonadError Error, MonadIO, MonadReader Handle)

-- | Make a request using 'Handle' and converting propagating failure codes
-- to the ErrorT monad.
request :: URLString -> [CurlOption] -> Freesound CurlResponse
request url options = do
    curl <- ask
    resp <- liftIO $ do_curl curl url (defaultCurlOptions ++ options)
    case respCurlCode resp of
        CurlOK -> return resp
        code   -> throwError (CurlError code)

-- | Make a request and parse the returned XML data.
requestXML :: URLString -> [CurlOption] -> Freesound XML.Element
requestXML url options = do
    resp <- request url options
    case XML.parseXMLDoc (respBody resp) of
        Nothing  -> throwError XMLError
        Just xml -> return xml

-- | Log into freesound.
login :: String -> String -> Freesound ()
login user password = do
    resp <- request loginURL opts
    -- Check for login success (duh!)
    -- TODO: Figure out a better way
    case findString "logged" (respBody resp) of
        Nothing -> throwError LoginError
        _       -> return ()
    where
        post = URL.postFields [
                ("username", user),
                ("password", password),
                ("login", "login"),
                ("redirect", loginRedirect) ]
        opts = [ CurlPostFields post,
                 -- CurlCookieJar cookieFile,
                 CurlFollowLocation True ]

-- | Log into Freesound with and perform an action in the 'Freesound' monad.
withFreesound :: String -> String -> Freesound a -> IO (Either Error a)
withFreesound user password f =
    withCurlDo $ liftIO Curl.initialize >>= runErrorT . runReaderT action
    where action = fromFreesound (login user password >> f)

-- | Search the Freesound database.
search :: Query -> Freesound [Sample]
search query = Sample.listFromXML `fmap` requestXML searchURL opts
    where opts = [ CurlPostFields (Query.toPostFields query) ]

-- | Similarity type used by 'searchSimilar'.
data Similarity = Similar | Dissimilar deriving (Eq, Show)

-- | Search samples similar (or dissimilar) to a 'Sample'.
searchSimilar :: Similarity -> Sample -> Freesound [Sample]
searchSimilar similarity sample = Sample.listFromXML `fmap` requestXML url []
    where
        url = URL.addParams params searchSimilarURL
        params = [ ("id", show (sampleId sample)),
                   ("inverse", case similarity of
                                Similar    -> "false"
                                Dissimilar -> "true") ]

-- | Get the properties of a 'Sample' as an XML document.
propertiesXML :: Sample -> Freesound XML.Element
propertiesXML sample = requestXML url []
    where url = URL.addParams [("id", show (sampleId sample))] xmlURL

-- | Get the properties of a 'Sample'.
properties :: Sample -> Freesound Properties
properties sample = do
    xml <- propertiesXML sample
    let props = Properties.listFromXML xml
    case find ((== sampleId sample) . Properties.sampleId) props of
        Just p  -> return p
        Nothing -> throwError $ Error ("Properties for sample " ++ (show $ sampleId sample) ++ " not found")

-- | Download a 'Sample' as a 'String'.
download :: Sample -> Freesound String
download sample = respBody `fmap` request url []
    where url = URL.addParams [("id", show (sampleId sample))] audioURL
