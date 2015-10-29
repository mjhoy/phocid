module Photo
       ( copiedPhotoPath
       , isPhotoPath
       ) where

import System.FilePath
import Data.Char (toLower)

import Types

copiedPhotoPath :: Photo -> FilePath
copiedPhotoPath photo =
  "./images" </> getPath photo

isPhotoPath :: FilePath -> Bool
isPhotoPath path =
  or $ map (== ext) [ ".jpg",
                      ".jpeg" ]
  where
    ext = map toLower $ takeExtension path
