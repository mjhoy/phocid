module Types ( Phocid(..)
             , Photo(..)
             ) where

data Phocid = Phocid
  { inputDir   :: FilePath
  , outputPath :: FilePath
  , verbose :: Bool
  , title :: String
  } deriving (Show)

data Photo = Photo
  { getPath :: FilePath }
  deriving (Show, Eq)

instance Ord Photo where
  p1 `compare` p2 = (getPath p1) `compare` (getPath p2)

