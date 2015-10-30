{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

------------------------------------------------------------------------------
import Options.Applicative
------------------------------------------------------------------------------
import Phocid (runWithPhocid, phocid)
------------------------------------------------------------------------------

main :: IO ()
main = customExecParser p opts >>= runWithPhocid
  where
    p = prefs showHelpOnError
    opts = info (helper <*> phocid)
           ( fullDesc
             <> header "phocid -- make simple html sites from photos" )
