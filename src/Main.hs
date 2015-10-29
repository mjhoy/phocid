{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

------------------------------------------------------------------------------
import Options.Applicative
import System.Directory
import System.FilePath
import System.Exit
------------------------------------------------------------------------------
import Types
import Photo
import Template
------------------------------------------------------------------------------

cmd :: Parser Cmd
cmd = Cmd
      <$> ( argument str
            ( metavar "INPUT_DIR"
              <> help "Directory to scan for photos" ) )
      <*> ( argument str
            ( metavar "OUTPUT_DIR"
              <> help "Output directory for site files" ))

run' :: Cmd -> [Photo] -> IO ()
run' c photos = do
  absInDir <- makeAbsolute $ inputDir c
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
  where
    out = outputPath c

photoFromPath :: FilePath -> IO Photo
photoFromPath path = do
  return $ Photo path

checkPhotos :: FilePath -> IO [Photo]
checkPhotos inDir = do
  contents <- getDirectoryContents inDir
  photos <- mapM photoFromPath $ filter isPhotoPath contents
  let photoN = length photos
      photoS = if photoN == 1 then "photo" else "photos"
  if photoN == 0
    then die $ "No photos found in " ++ inDir
    else do
      putStrLn $ "OK, " ++ (show photoN) ++ " " ++ photoS ++ "  found"
      return photos

run :: Cmd -> IO ()
run c = checkPhotos (inputDir c) >>= run' c

main :: IO ()
main = customExecParser p opts >>= run
  where
    p = prefs showHelpOnError
    opts = info (helper <*> cmd)
           ( fullDesc
             <> header "phocin -- make simple html sites from photos" )
