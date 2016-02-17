{-# LANGUAGE OverloadedStrings #-}
module Sound.Freesound.UserSpec (spec) where

import           Data.Default (def)
import qualified Data.Set as Set
import           Sound.Freesound
import qualified Sound.Freesound.Sound as Sound
import qualified Sound.Freesound.User as User
import           Sound.Freesound.Test
import           Test.Hspec

k0s :: IO User.User
k0s = fs $ User.getUserByName "k0s"

spec :: Spec
spec = do
  describe "User" $ do
    it "can be retrieved by name" $ do
      u <- k0s
      User.username u `shouldBe` "k0s"
    it "has the correct date" $ do
      u <- k0s
      User.dateJoined u `shouldBe` read "2007-02-02 17:32:34 UTC"
    it "has the correct number of sounds" $ do
      u <- k0s
      sounds <- fs $ getResource (User.sounds u)
      numElems sounds `shouldBe` User.numSounds u
    it "has the correct set of sounds" $ do
      u <- k0s
      sounds <- fs $ getAll =<< getResource (User.sounds u)
      sounds' <- fs $ getAll =<< search def def (username "k0s") def
      Set.fromList (map Sound.id sounds) `shouldBe` Set.fromList (map Sound.id sounds')
