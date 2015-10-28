{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import System.Directory
import System.FilePath
import System.IO
import System.Exit
import Data.Char (toLower)

import Text.Hamlet
import Text.Lucius
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html (toHtml)

data Cmd = Cmd
  { inputDir  :: FilePath
  , outputPath :: Maybe FilePath }
  deriving (Show)

data Photo = Photo
  { getPath :: FilePath }
  deriving (Show)

cmd :: Parser Cmd
cmd = Cmd
      <$> ( argument str
            ( metavar "INPUT_DIR"
              <> help "Directory to scan for photos" ) )
      <*> ( optional ( argument str
                       ( metavar "OUTPUT_DIR"
                         <> help "Output directory for site files" )))

people :: [String]
people = ["one"]

style :: Html
style = toHtml $ renderCss $ [lucius|
body{background:#EBEBEB}
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

copiedPhotoPath :: Photo -> FilePath
copiedPhotoPath photo =
  "./images" </> getPath photo

photoTest :: FilePath -> Bool
photoTest path =
  or $ map (== ext) [ ".jpg",
                      ".jpeg" ]
  where
    ext = map toLower $ takeExtension path

renderIndex :: [Photo] -> String
renderIndex photos = renderHtml $ htmlTemplate "Title Here" [shamlet|
<ul class=photos>
  $forall photo <- photos
    ^{photoTemplate photo}
|]

-- run' fails if outputDir is Nothing
run' :: Cmd -> [Photo] -> IO ()
run' c photos = do
  absInDir <- makeAbsolute $ inputDir c
  case outputPath c of
    Just out -> do
      outDirExists <- doesDirectoryExist out
      if outDirExists
        then die $ "Directory exists: " ++ out
        else do
          absOutDir <- makeAbsolute out
          createDirectory absOutDir
          let absImgDir = absOutDir </> "images"
              photoPaths = map getPath photos
          createDirectory absImgDir
          mapM_ (\file -> (absInDir </> file) `copyFile` (absImgDir </> file)) photoPaths
          let html = renderIndex photos
          writeFile (absOutDir </> "index.html") html
    Nothing  -> do
      die "couldn't set outputDir"

-- helper function
expandFilePath :: FilePath -> IO FilePath
expandFilePath path = do
  homeDir <- getHomeDirectory
  case path of
    ('~':'/':xs) -> return $ homeDir </> xs
    ('~':[])     -> return homeDir
    x -> return x

photoFromPath :: FilePath -> IO Photo
photoFromPath path = do
  return $ Photo path

checkPhotos :: FilePath -> IO [Photo]
checkPhotos inDir = do
  contents <- getDirectoryContents inDir
  photos <- mapM photoFromPath $ filter photoTest contents
  let photoN = length photos
      photoS = if photoN == 1 then "photo" else "photos"
  if photoN == 0
    then die $ "No photos found in " ++ inDir
    else do
      putStrLn $ "OK, " ++ (show photoN) ++ " " ++ photoS ++ "  found"
      return photos

run :: Cmd -> IO ()
run c = do
  case outputPath c of
    Just _ -> checkPhotos (inputDir c) >>= run' c
    Nothing  -> do
      photos <- checkPhotos (inputDir c)
      askForOutputDirectory >>= \x -> run' (c { outputPath = Just x }) photos

askForOutputDirectory :: IO String
askForOutputDirectory = do
  home <- getHomeDirectory
  let def = home </> "export"
  putStr $ "Enter an output directory [" ++ def ++ "]: "
  hFlush stdout
  dir <- getLine
  case dir of
    [] -> return def
    x  -> expandFilePath x

main :: IO ()
main = customExecParser p opts >>= run
  where
    p = prefs showHelpOnError
    opts = info (helper <*> cmd)
           ( fullDesc
             <> header "phocin -- make simple html sites from photos" )
