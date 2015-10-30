{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

------------------------------------------------------------------------------
import Test.Hspec
import Text.Lucius
import Text.Blaze.Html.Renderer.Text (renderHtml)
import System.Directory
import System.Posix.Files
import Data.List
------------------------------------------------------------------------------
import Types
import Photo (isPhotoPath)
import Style (cssToHtml)
import Phocid  (runWithPhocid)
------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do

  let removeTmpDir = removeDirectoryRecursive "/tmp/export"
  let testPhocid = Phocid { inputDir  = "test/test_photo_dir"
                          , outputPath = "/tmp/export"
                          , verbose = False }
  let testRunP = runWithPhocid testPhocid

  describe "Test run" $ before_ testRunP $ after_ removeTmpDir $ do

      it "should produce an export dir" $ do
        dirExists <- fileExist "/tmp/export"
        dirExists `shouldBe` True

      it "should copy a photo" $ do
        photoExists <- fileExist "/tmp/export/images/P1020299.jpg"
        photoExists `shouldBe` True

      it "should produce an index" $ do
        indexExists <- fileExist "/tmp/export/index.html"
        indexExists `shouldBe` True

      it "should have a reference to a photo within the index" $ do
        index <- readFile "/tmp/export/index.html"
        ("P1020299" `isInfixOf` index) `shouldBe` True

  describe "Photo.isPhotoPath" $ do
    it "is true for jpgs, false for others" $ do
      isPhotoPath "test.jpg" `shouldBe` True

  describe "Style.cssToHtml" $ do
    it "escapes properly" $ do
      renderHtml (cssToHtml testCss) `shouldBe` "body{font-family:\"Helvetica\"}"

testCss :: Css
testCss = [lucius|body { font-family: "Helvetica"; }|] undefined

