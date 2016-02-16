{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM, mzero)
import Data.Aeson
import Data.Text (Text)
import Sound.Freesound.API (Freesound, Resource, getResource)

data List a = List {
  elems    :: [a]
, numElems :: Int
, previous :: Maybe (Resource (List a))
, next     :: Maybe (Resource (List a))
} deriving (Eq, Show)

class Elem a where
  elemsFieldName :: a -> Text

instance (Elem a, FromJSON a) => FromJSON (List a) where
  parseJSON (Object v) =
    List
      <$> v .: "results"
      <*> v .: "count"
      <*> v .:? "previous"
      <*> v .:? "next"
  parseJSON _ = mzero

getPrevious :: (Elem a, FromJSON a) => List a -> Freesound (Maybe (List a))
getPrevious = maybe (return Nothing) (liftM Just . getResource) . previous

getNext :: (Elem a, FromJSON a) => List a -> Freesound (Maybe (List a))
getNext = maybe (return Nothing) (liftM Just . getResource) . next
