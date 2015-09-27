import System.Environment(getArgs, getProgName)
import System.Exit(exitFailure)
import Control.Monad(when)
import Denominate

usage = getProgName >>= \name ->
          return ("Usage: " ++ name ++ " [-h|-n] base_directory\n" ++
                  "  -h: show help\n" ++
                  "  -n: dry run; show what would be renamed") >>=
          putStrLn >> exitFailure


main = do
  getArgs >>= \args ->
    when (doUsageAndExit args) usage >>
      let forReal = not(elem "-n" args)
          pathArgs = filter (\s -> not(elem s ["-n", "-h", "--help"])) args
      in (fileToTypedFilePath . base) pathArgs >>=
           renameAll forReal defaultFilenameConverter >>=
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

doUsageAndExit args = null args || elem "-h" args || elem "--help" args
