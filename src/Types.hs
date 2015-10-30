module Types ( Phocid(..)
             , Photo(..)
             ) where

data Phocid = Phocid
  { inputDir   :: FilePath
  , outputPath :: FilePath
  , verbose :: Bool
  } deriving (Show)

data Photo = Photo
  { getPath :: FilePath }
  deriving (Show)

