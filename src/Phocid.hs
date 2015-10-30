module Phocid ( phocid
              , runWithPhocid
              ) where

------------------------------------------------------------------------------
import Options.Applicative
import System.Directory
import System.FilePath
import System.Exit
------------------------------------------------------------------------------
import Types (Phocid(..), Photo(..))
import Template
import Photo
------------------------------------------------------------------------------

phocid :: Parser Phocid
phocid = Phocid
      <$> ( argument str
            ( metavar "INPUT_DIR"
              <> help "Directory to scan for photos" ) )
      <*> ( argument str
            ( metavar "OUTPUT_DIR"
              <> help "Output directory for site files" ))
      <*> ( switch
            ( long "verbose"
              <> help "Verbose output" ))

runWithPhocid :: Phocid -> IO ()
runWithPhocid p = do
  absInDir <- makeAbsolute $ inputDir p
  photos <- checkPhotos (verbose p) absInDir
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
    out = outputPath p
