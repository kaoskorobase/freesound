{-# LANGUAGE CPP, OverloadedStrings #-}
module Sound.Freesound.Pack.Type (
  Pack(..)
) where

import           Data.Aeson (FromJSON(..), Value(..), (.:))
import           Data.Aeson.Types (typeMismatch)
import           Data.Text (Text)
import           Sound.Freesound.API (URI)
import           Sound.Freesound.Time

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif

newtype PackId = PackId Integer deriving (Eq, Ord, Show)

instance FromJSON PackId where
  parseJSON (Number i) = return $ PackId (truncate i)
  parseJSON v = typeMismatch "PackId" v

data Pack = Pack {
    id :: PackId        -- ^ The unique identifier of this pack.
  , url :: URI          -- ^ The URI for this pack on the Freesound website.
  , description :: Text -- ^ The description the user gave to the pack (if any).
  , created :: UTCTime  -- ^ The date when the pack was created (e.g. “2014-04-16T20:07:11.145”).
  , name :: Text        -- ^ The name user gave to the pack.
  , username :: Text    -- ^ Username of the creator of the pack.
  , numSounds :: Int    -- ^ The number of sounds in the pack.
  , sounds :: URI       -- ^ The URI for a list of sounds in the pack. Plain URI in order to break module dependency cycle.
  , numDownloads :: Int -- ^ The number of times this pack has been downloaded.
} deriving (Eq, Show)

instance FromJSON Pack where
  parseJSON (Object v) =
    Pack
      <$> v .: "id"
      <*> v .: "url"
      <*> v .: "description"
      <*> (toUTCTime <$> (v .: "created"))
      <*> v .: "name"
      <*> v .: "username"
      <*> v .: "num_sounds"
      <*> v .: "sounds"
      <*> v .: "num_downloads"
  parseJSON v = typeMismatch "Pack" v
