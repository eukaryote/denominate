module Main where

import System.Environment(getArgs, getProgName)
import System.Exit(exitFailure)
import Control.Monad(when)
import Text.Printf
import Denominate

usage = getProgName >>= \name ->
          return ("Usage: " ++ name ++ " base_directory") >>=
          putStrLn >> exitFailure

main = do
  getArgs >>= \args ->
    when (doUsageAndExit args) usage >>
      (fileToTypedFilePath . base) args >>=
         renameAll defaultFilenameConverter >>=
            mapM_ handle

-- Get base dir to recurse in and ensure there is no terminal '/'.
base (d:ds) =
  case d == "/" of
    True    -> d
    False   -> if (head revD) /= '/' 
                  then d
                  else reverse $ drop 1 revD
  where revD = reverse d

handle result =
  case result of
    (Failure (fType, fName) msg) -> putStr msg >> putStr " [" >>
                                    putStr fName >> putStrLn "]"
    (Success (fType, fName) msg) -> return ()

doUsageAndExit args = null args || arg0 == "-h" || arg0 == "--help"
  where arg0 = head args
