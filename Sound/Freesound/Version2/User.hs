module Sound.Freesound.Version2.User (
    Info(..)
) where

import Data.Text (Text)
import Network.URI (URI)

-- | User of the Freesound database.
data Info = Info {
    username :: String  -- | The user’s username.
  , ref  :: URI 		-- | The URI for this resource.
  , url :: URI 			-- | The profile page for the user on the Freesound website.
  } deriving (Eq, Show)

data User = User {
	info :: Info
  , sounds :: URI		-- | The API URI for this user’s sound collection.
  , packs :: URI		-- | The API URI for this user’s pack collection.
  , first_name :: Text 	-- | The user’s first name, possibly empty.
  , last_name :: Text   -- | The user’s last name, possibly empty.
  , about :: Text 		-- | A small text the user wrote about himself.
  , home_page :: URI	-- | The user’s homepage, possibly empty.
  , signature :: Text   -- | The user’s signature, possibly empty.
  , dateJoined :: Text  -- | The date the user joined Freesound.
  } deriving (Eq, Show)
