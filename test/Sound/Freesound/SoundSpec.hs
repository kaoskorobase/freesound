{-# LANGUAGE CPP, OverloadedStrings #-}
module Sound.Freesound.SoundSpec (spec) where

import           Data.List (find)
import           Sound.Freesound
import qualified Sound.Freesound.Sound as Sound
import           Sound.Freesound.Test
import           Test.Hspec

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif

spiffySpankId :: Sound.SoundId
spiffySpankId = Sound.SoundId 167068

spiffySpank :: Freesound Sound.Detail
spiffySpank = Sound.soundById spiffySpankId

nokia_2600_warmup :: Freesound Sound.Detail
nokia_2600_warmup = Sound.soundById (Sound.SoundId 330864)

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
    it "can be recovered from its summary" $ do
      Just info <- find (\s -> Sound.id s == spiffySpankId)
                    <$> (fs $ getAll =<< search_ (include "Spiffy Spank"))
      (s, s') <- fs $ (,) <$> Sound.soundDetail info <*> spiffySpank
      s `shouldBe` s'
    it "has no geotag" $ do
      Sound.geotag <$> fs spiffySpank `shouldReturn` Nothing
    it "has the correct geotag" $ do
      Sound.geotag <$> fs nokia_2600_warmup
        `shouldReturn` Just (Sound.GeoTag 41.4151829276 2.16533660889)
