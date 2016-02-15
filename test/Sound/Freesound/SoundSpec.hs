{-# LANGUAGE OverloadedStrings #-}
module Sound.Freesound.SoundSpec (spec) where

import Data.Default (def)
import Sound.Freesound as FS
import System.Environment (getEnv)
import Test.Hspec

fs a = do
  k <- apiKeyFromString <$> getEnv "API_KEY"
  runFreesound k a

spec :: Spec
spec = do
  describe "search" $ do
    it "searches" $ fs $ do
      let q =   FS.include "folks"
            -- & FS.include "stream"
            -- & FS.exclude "birds"
            -- & FS.exclude "insects"
            -- & FS.exclude "highway"
          f = FS.samplerate (FS.greaterThan 44100)
      FS.search def def f q
      return ()
