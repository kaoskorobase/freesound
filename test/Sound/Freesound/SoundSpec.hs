{-# LANGUAGE OverloadedStrings #-}
module Sound.Freesound.SoundSpec (spec) where

import           Data.Default (def)
import           Sound.Freesound
import qualified Sound.Freesound.Sound as Sound
import           Sound.Freesound.Test
import           Test.Hspec

spec :: Spec
spec = do
  describe "search" $ do
    it "searches" $ do
      let p s =    Sound.username s == "k0s"
                && Sound.name s == "Spiffy Spank"
      fs (anySatisfy p =<< search_ (include "spank")) `shouldReturn` True
    it "filters by user name" $ do
      let p s = Sound.username s == "k0s"
      fs (allSatisfy p =<< search def def (username "k0s") (include "spank")) `shouldReturn` True
    it "paginates" $ do
      l <- fs $ search (Pagination 0 1) def def ""
      numElems l `shouldBe` 1
