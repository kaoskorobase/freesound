{-# LANGUAGE OverloadedStrings #-}
module Sound.Freesound.UserSpec (spec) where

import           Data.Default (def)
import           Sound.Freesound
import qualified Sound.Freesound.User as User
import           Sound.Freesound.Test
import           Test.Hspec

spec :: Spec
spec = do
  describe "User" $ do
    it "can be retrieved by name" $ do
      u <- fs $ User.getUserByName "k0s"
      print u
      User.username u `shouldBe` "k0s"
