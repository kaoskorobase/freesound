module Sound.Freesound.Query (
    Query(..),
    stringQuery,
    toPostFields
) where

import qualified Sound.Freesound.URL as URL

-- | A 'Query' type for searching the database.
data Query = Query {
    string      :: String,
    minDur      :: Maybe Double,
    maxDur      :: Maybe Double,
    bitRate     :: Maybe Int,
    bitDepth    :: Maybe Int,
    sampleRate  :: Maybe Int
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
