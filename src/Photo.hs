module Photo
       ( copiedPhotoPath
       , photoTest
       ) where

import System.FilePath
import Data.Char (toLower)

import Types

copiedPhotoPath :: Photo -> FilePath
copiedPhotoPath photo =
  "./images" </> getPath photo

photoTest :: FilePath -> Bool
photoTest path =
  or $ map (== ext) [ ".jpg",
                      ".jpeg" ]
  where
    ext = map toLower $ takeExtension path
