module Main where

import System.Environment(getArgs, getProgName)
import System.Exit(exitFailure)
import Control.Monad(when)
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
base [] = error "Main.base"
base (d:_) =
  case d == "/" of
    True    -> d
    False   -> if (head revD) /= '/' 
                  then d
                  else reverse $ tail revD
  where revD = reverse d

handle result =
  case result of
    (Failure (_, fName) msg) -> putStr msg >> putStr " [" >>
                                    putStr fName >> putStrLn "]"
    (Success  _         _  )  -> return ()

doUsageAndExit args = null args || arg0 == "-h" || arg0 == "--help"
  where arg0 = head args
