{-# LANGUAGE CPP, OverloadedStrings #-}
module Sound.Freesound.SoundSpec (spec) where

import qualified Data.ByteString.Lazy as BL
import           Data.List (find)
import           Data.Maybe (fromJust)
import           Sound.Freesound
import qualified Sound.Freesound.Comment as Comment
import qualified Sound.Freesound.Pack as Pack
import           Sound.Freesound.Search
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
  describe "Spiffy Spank" $ do
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
    it "is not part of a pack" $ do
      Sound.pack <$> fs spiffySpank `shouldReturn` Nothing
  describe "nokia_2600_warmup" $ do
    it "has the correct geotag" $ do
      Sound.geotag <$> fs nokia_2600_warmup
        `shouldReturn` Just (Sound.GeoTag 41.4151829276 2.16533660889)
    it "is part of pack \"Nokia 2600\"" $ do
      r <- Sound.pack <$> fs nokia_2600_warmup
      r `shouldNotBe` Nothing
      p <- fs . get . fromJust $ r
      Pack.name p `shouldBe` "Nokia 2600"
    unlessCI $ do
      it "can be downloaded" $ do
        b <- fs $ download . Sound.download =<< nokia_2600_warmup
        b' <- BL.readFile "test/data/330864__k0s__nokia-2600-warmup.wav"
        b `shouldBe` b'
  describe "Nokia_2600_Silence.wav" $ do
    it "should have a lovely comment" $ do
      s <- fs . Sound.soundById $ Sound.SoundId 231519
      l <- fs . get . Sound.comments $ s
      let c = Comment.Comment {
            Comment.username = "toiletrolltube"
          , Comment.comment = "I love this sort of stuff, very useful indeed thanks."
          , Comment.created = read "2014-03-29 03:15:55.948 UTC" }
      fs (anySatisfy (==c) l) `shouldReturn` True
