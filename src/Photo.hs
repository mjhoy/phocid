module Photo
       ( copiedPhotoPath
       , isPhotoPath
       , checkPhotos
       ) where

------------------------------------------------------------------------------
import System.Directory
import System.FilePath
import System.Exit
import Data.Char (toLower)
------------------------------------------------------------------------------

import Types

checkPhotos :: Bool ->     -- verbose
               FilePath -> -- directory to look in
               IO [Photo]  -- parse and return Photo objects
checkPhotos v inDir = do
  contents <- getDirectoryContents inDir
  photos <- mapM photoFromPath $ filter isPhotoPath contents
  let photoN = length photos
      photoS = if photoN == 1 then "photo" else "photos"
  if photoN == 0
    then die $ "No photos found in " ++ inDir
    else do
      if v
        then putStrLn $ "OK, " ++ (show photoN) ++ " " ++ photoS ++ "  found"
        else return ()
      return photos

photoFromPath :: FilePath -> IO Photo
photoFromPath path = do
  return $ Photo path

copiedPhotoPath :: Photo -> FilePath
copiedPhotoPath photo =
  "./images" </> getPath photo

isPhotoPath :: FilePath -> Bool
isPhotoPath path =
  or $ map (== ext) [ ".jpg",
                      ".jpeg" ]
  where
    ext = map toLower $ takeExtension path
