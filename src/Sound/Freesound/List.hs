{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables #-}
module Sound.Freesound.List (
  List
, Elem(..)
, elems
, numElems
, previous
, next
, getPrevious
, getNext
) where

import Control.Monad (liftM, mzero)
import Data.Aeson
import Data.Text (Text)
import Sound.Freesound.API (Freesound, Resource, getResource)

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif

data List a = List {
  elems    :: [a]
, numElems :: Int
, previous :: Maybe (Resource (List a))
, next     :: Maybe (Resource (List a))
} deriving (Eq, Show)

class Elem a where
  elemsFieldName :: a -> Text

instance FromJSON a => FromJSON (List a) where
  parseJSON (Object v) =
    List
      <$> v .: "results"
      <*> v .: "count"
      <*> v .:? "previous"
      <*> v .:? "next"
  parseJSON _ = mzero

getPrevious :: FromJSON a => List a -> Freesound (Maybe (List a))
getPrevious = maybe (return Nothing) (liftM Just . getResource) . previous

getNext :: FromJSON a => List a -> Freesound (Maybe (List a))
getNext = maybe (return Nothing) (liftM Just . getResource) . next
