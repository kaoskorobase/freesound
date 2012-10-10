module Sound.Freesound.Query (
    Query(..),
    stringQuery,
    toPostFields
) where

import qualified Sound.Freesound.URL as URL

-- | A 'Query' type for searching the database.
data Query = Query {
    string      :: String,          -- ^ The query string
    minDur      :: Maybe Double,    -- ^ Minimum duration in seconds
    maxDur      :: Maybe Double,    -- ^ Maximum duration in seconds
    bitRate     :: Maybe Int,       -- ^ Bit rate of the soundfile
    bitDepth    :: Maybe Int,       -- ^ Bit depth of the soundfile
    sampleRate  :: Maybe Int        -- ^ Sample rate of the soundfile
}

-- | Construct a 'Query' to search the database for a 'String'.
stringQuery :: String -> Query
stringQuery s = Query s Nothing Nothing Nothing Nothing Nothing

-- | Convert a query to POST request fields.
toPostFields :: Query -> [String]
toPostFields query = URL.postFields [ x | Just x <- [
                        Just ("search", string query),
                        fmap ((,) "durationMin" . show) (minDur query),
                        fmap ((,) "durationMax" . show) (maxDur query),
                        fmap ((,) "bitrate"     . show) (bitRate query),
                        fmap ((,) "bitdepth"    . show) (bitDepth query),
                        fmap ((,) "samplerate"  . show) (sampleRate query) ] ]
