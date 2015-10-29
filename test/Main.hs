{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

------------------------------------------------------------------------------
import Test.Hspec
import Text.Lucius
import Text.Blaze.Html.Renderer.Text (renderHtml)
------------------------------------------------------------------------------
import Photo (isPhotoPath)
import Style (cssToHtml)
------------------------------------------------------------------------------

testCss :: Css
testCss = [lucius|
body { font-family: "Helvetica"; }
|] undefined

main :: IO ()
main = hspec $ do

  describe "Photo.isPhotoPath" $ do
    it "is true for jpgs, false for others" $ do
      isPhotoPath "test.jpg" `shouldBe` True

  describe "Style.cssToHtml" $ do
    it "escapes properly" $ do
      renderHtml (cssToHtml testCss) `shouldBe` "body{font-family:\"Helvetica\"}"
