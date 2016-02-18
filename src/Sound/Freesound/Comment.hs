{-# LANGUAGE CPP, OverloadedStrings #-}
module Sound.Freesound.Comment (
  Comment(..)
) where

import           Data.Aeson (FromJSON(..), Value(..), (.:))
import           Data.Aeson.Types (typeMismatch)
import           Data.Text (Text)
import           Sound.Freesound.Time

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif

data Comment = Comment {
  username :: Text
  , comment :: Text
  , created :: UTCTime
  } deriving (Eq, Show)

instance FromJSON Comment where
  parseJSON (Object v) =
    Comment <$> v .: "username"
            <*> v .: "comment"
            <*> (toUTCTime <$> (v .: "created"))
  parseJSON v = typeMismatch "Comment" v
