{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Style (style) where

------------------------------------------------------------------------------
import Text.Lucius
import Text.Blaze.Html (toHtml, Html)
------------------------------------------------------------------------------

style :: Html
style = toHtml $ renderCss $ [lucius|

body {
  background: #EBEBEB;
}

ul.photos {
  list-style: none;
  margin: 0 20px;
  padding: 0;
}

li {
  margin: 150px 20px;
  text-align: center;
}
li:first-child {
  margin-top: 40px;
}

img { max-width: 100%; }

h1 {
  margin: 40px 20px 20px 20px;
  text-align: center;
  font-family: "Helvetica Neue", Helvetica, sans-serif;
  font-weight: 500;
}

|] undefined
