{-# LANGUAGE OverloadedStrings #-}
module Sound.Freesound.SoundSpec (spec) where

import           Sound.Freesound
import qualified Sound.Freesound.Sound as Sound
import           Sound.Freesound.Test
import           Test.Hspec

spiffySpank :: Freesound Sound.Detail
spiffySpank = Sound.soundById (Sound.SoundId 167068)

spec :: Spec
spec = do
  describe "Sound" $ do
    it "can be retrieved by id" $ do
      s <- fs spiffySpank
      Sound.name s `shouldBe` "Spiffy Spank"
    it "has the correct creation date" $ do
      s <- fs spiffySpank
      Sound.created s `shouldBe` read "2012-10-10 15:24:33.328 UTC"
    it "has the correct file type" $ do
      s <- fs spiffySpank
      Sound.fileType s `shouldBe` Sound.WAV
