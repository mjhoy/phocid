{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Template (renderIndex) where

------------------------------------------------------------------------------
import Text.Hamlet
import Text.Blaze.Html.Renderer.String (renderHtml)
------------------------------------------------------------------------------
import Style
import Photo
import Types
------------------------------------------------------------------------------

renderIndex :: [Photo] -> String
renderIndex photos = renderHtml $ htmlTemplate "Title Here" [shamlet|
<ul class=photos>
  $forall photo <- photos
    ^{photoTemplate photo}
|]

htmlTemplate :: String ->       -- title
                Html ->         -- body
                Html
htmlTemplate title body = [shamlet|
$doctype 5
<html>
  <head>
    <style>
      ^{style}
    <title>
      #{title}
    <body>
      <h1>
        #{title}
      ^{body}
|]

photoTemplate :: Photo -> Html
photoTemplate photo = [shamlet|
<li class=photo>
  <a href=#{path}>
    <img src=#{path}>
|]
  where path = copiedPhotoPath photo

