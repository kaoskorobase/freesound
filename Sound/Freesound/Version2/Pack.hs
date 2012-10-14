module Sound.Freesound.Version2.Pack (
  Pack(..)
) where

import           Data.Text (Text)
import           Network.URI (URI)
import qualified Sound.Freesound.Version2.User as User

data Pack = Pack {
  ref :: Resource Pack        -- | The URI for this resource.
, url :: Data                 -- | The URL for this pack’s page on the Freesound website.
, sounds :: Resource Sounds   -- | The API URI for the pack’s sound collection.
, user :: Resource User.Info  -- | A JSON object with the user’s username, url, and ref.
, name :: Text                -- | The pack’s name.
, created :: Text             -- | The date when the pack was created.
, numDownloads :: Integer     -- | The number of times the pack was downloaded. 
} deriving (Eq, Show)
