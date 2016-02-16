{-# LANGUAGE OverloadedStrings #-}
module Sound.Freesound.SoundSpec (spec) where

import Sound.Freesound
import Sound.Freesound.Test
import Test.Hspec

spec :: Spec
spec = do
  describe "search" $ do
    it "searches" $ fs $ do
      search_ (include "")
      return ()
