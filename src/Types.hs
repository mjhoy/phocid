module Types
       ( Cmd(..)
       , Photo(..)
         ) where

data Cmd = Cmd
  { inputDir  :: FilePath
  , outputPath :: Maybe FilePath }
  deriving (Show)

data Photo = Photo
  { getPath :: FilePath }
  deriving (Show)

