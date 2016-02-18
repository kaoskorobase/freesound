{-# LANGUAGE CPP, OverloadedStrings #-}
module Sound.Freesound.UserSpec (spec) where

import           Data.Default (def)
import           Data.List (find)
import           Data.Maybe (fromJust)
import qualified Data.Set as Set
import           Sound.Freesound
import qualified Sound.Freesound.Bookmark as Bookmark
import qualified Sound.Freesound.Sound as Sound
import qualified Sound.Freesound.User as User
import           Sound.Freesound.Test
import           Test.Hspec

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif

k0s :: Freesound User.User
k0s = User.getUserByName "k0s"

spec :: Spec
spec = do
  describe "User" $ do
    it "can be retrieved by name" $ do
      u <- fs k0s
      User.username u `shouldBe` "k0s"
    it "has the correct date" $ do
      u <- fs k0s
      User.dateJoined u `shouldBe` read "2007-02-02 17:32:34 UTC"
    it "has the correct number of sounds" $ do
      u <- fs k0s
      sounds <- fs $ getResource $ User.sounds u
      numElems sounds `shouldBe` User.numSounds u
    it "has the correct set of sounds" $ do
      sounds <- fs $ getAll =<< getResource . User.sounds =<< k0s
      sounds' <- fs $ getAll =<< search def def (username "k0s") def
      Set.fromList (map Sound.id sounds) `shouldBe` Set.fromList (map Sound.id sounds')
    -- it "has a pack" $ do
    --   p <- fs $ getResource =<< User.packs <$> User.getUserByName "silentseason"
    --   print p
    it "has haskell-freesound bookmark category" $ do
      l <- fs $ getResource . User.bookmarkCategories =<< k0s
      -- More than one category
      numElems l `shouldSatisfy` (>=1)
      -- Test bookmark category
      b <- fs $ find (\b -> Bookmark.name b == "haskell-freesound-tests") <$> getAll l
      b `shouldNotBe` Nothing
      -- Number of sounds in category
      Bookmark.numSounds (fromJust b) `shouldBe` 1
      sounds <- fs . getResource . Bookmark.sounds . fromJust $ b
      numElems sounds `shouldBe` 1
      -- Correct sound in category
      elems sounds `shouldSatisfy` (not.null)
      Sound.id (head (elems sounds)) `shouldBe` Sound.SoundId 250719
