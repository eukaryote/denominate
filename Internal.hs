module Internal where

import System.Directory
import System.FilePath
import System.IO()
import Data.Char
import Data.List
import Control.Monad
import Control.Exception

-- |Represents the type of a file or directory. These are exhaustive,
-- as for purposes of this module, we consider everything that isn't
-- a directory to be a file.
data FileType = Directory | File
  deriving (Eq, Show, Enum, Ord)

-- |Represents the result of a rename attempt, which either fails or
-- succeeds. In both cases, the typed file path is that of the file
-- for which a rename attempt was made. Upon failure, the string
-- parameter contains information about the error (may have been an
-- os-level error or user error); upon success, the string parameter
-- is the name to which the file was renamed (which includes the case
-- that no change was performed because old and new names were equal).
data RenameResult = Failure TypedFilePath String |
                    Success TypedFilePath String
  deriving (Eq, Show)

-- |Represents a filepath together with the type of file to which
-- the path refers.
type TypedFilePath = (FileType, FilePath)

-- |A filename converter maps an old filename to a new filename. A converter
-- takes a typed file representing a directory name or a filename without
-- extension, and only needs to determine the new name based on the old name.
-- It does not need to worry about extracting the file path from an absolute
-- path or determining the file extension, as all functions in this module
-- that use a FilenameConverter will only pass in typed files containing
-- the last directory (if a directory) or the filename without extension
-- if a file.
type FilenameConverter = TypedFilePath -> FilePath

-- |Answer whether the two paths are the same, ignoring the presence of "./"
-- at the beginning of a path.
samePaths:: FilePath -> FilePath -> Bool
samePaths path1 path2 = norm path1 == norm path2
  where norm path = if take 2 path == "./" then drop 2 path else path

-- |Rename a single file or directory using the supplied filename converter,
-- which will be passed just the directory name (without any parent
-- directories or a terminal slash) in the case of a directory or just
-- the filename without the extension in the case of a file. If there
-- already exists a file with the intended new name, nothing is done. If
-- the new name is the same as the old name (not considering file extension),
-- then the function successfully returns without touching the filesystem.
-- In all cases where a file is renamed, the extension of a file will be
-- automatically converted to lowercase but otherwise remains the
-- same (no characters are ever removed from the extension).
rename :: Bool -> FilenameConverter -> TypedFilePath -> IO (RenameResult)
rename forReal convFunc f@(_, oldPath) =
  let
      newPath     =  normalizeFilename convFunc f
      fail f msg  =  return (Failure f msg)
  in
      case samePaths oldPath newPath of
        True -> return (Success f oldPath)
        _    -> do exists <- fileExists newPath
                   if exists
                      then fail f "WARN: File already exists with new name."
                      else doRenameSafe forReal f newPath

-- |Renames the old file or directory to the new path, returning
-- a result that indicates success or failure. If successful,
-- the name of the new file is the success message; if unsuccessful,
-- the failure message gives more information.
doRenameSafe :: Bool -> TypedFilePath -> FilePath -> IO RenameResult
doRenameSafe forReal f newPath =
  handle ((\exc -> return (Failure f ("ERROR: " ++ show exc))) :: SomeException -> IO RenameResult)
         (doRename f newPath >> (return (Success f newPath)))
  where
    doRename :: TypedFilePath -> FilePath -> IO()
    doRename (fileType, oldPath) newPath =
      if forReal
        then case fileType of
               Directory -> renameDirectory oldPath newPath
               File      -> renameFile oldPath newPath
        else putStrLn ("Would rename: " ++ (normalizePath oldPath) ++ " -> " ++ newPath) >> return ()

-- |Rename all files and directories, recursively, in the given directory,
-- using the supplied filename converter to determine the new name of each
-- file or directory. The converter function will be called once for each
-- file or directory, and will be passed just the directory name (without
-- the parent directories) in the case of a directory or just the filename
-- without the extension in the case of a file. The extension of files (but
-- not directories if they seem to have an extension) will be converted to
-- lower case, but is not otherwise changed. There will be one RenameResult
-- for each success or failure, and an indication of the reason for failure
-- for failures, or the new name in case of success.
-- The `Bool` argument indicates whether to rename the files (true) or only
-- show what would be renamed (false).
renameAll :: Bool -> FilenameConverter -> TypedFilePath -> IO ([RenameResult])
renameAll forReal fn baseDir = (allFilepaths . snd) baseDir >>= mapM (rename forReal fn)

-- |Determine if the filename does not represent a dot file ("." or "..").
isNotDotFile :: FilePath -> Bool
isNotDotFile = not . flip elem [".", ".."]

-- |Determine whether there exists a file or directory with the given path.
fileExists :: FilePath -> IO Bool
fileExists path = do
  b1 <- doesDirectoryExist path
  b2 <- doesFileExist path
  return (b1 || b2)

-- |Generate a list of all files and directories below the given directory,
-- in depth-first order such that all files in a given directory appear
-- before the directory or any of its parent directories.
allFilepaths :: FilePath -> IO [TypedFilePath]
allFilepaths dir = do
  -- get [TypedFilePath] for files in this dir, dirs sorted before files
  dirTypedFilePaths <- getFilesAndDirectories dir
  -- get [TypedFilePath] for each subdir in this dir
  subDirTypedFilePaths <- mapM
                        (allFilepaths . snd) $
                        filter (isDirectoryFileType . fst) dirTypedFilePaths
  -- concatenate all the subdir paths with the paths from this dir
  return (prepend subDirTypedFilePaths dirTypedFilePaths)

-- |Get a list of files and directories, as TypedFilePath, for the given
-- directory, with directories sorted before files.
getFilesAndDirectories :: String -> IO [TypedFilePath]
getFilesAndDirectories dir =
    getDirectoryContents dir >>=
        filterM (return . isNotDotFile) >>=
        return . map (joinFileName dir) >>=
        mapM fileToTypedFilePath >>=
        return . sortPaths

-- |Converts a list of lists and a list into a single list,
-- ensuring that all items of the single list are *after*
-- all items from the list of lists. E.g.,
-- prepend [[1,2], [3,4], [5,6,7]] [66,67] == [1,2,3,4,5,6,7,66,67]
prepend :: [[a]] -> [a] -> [a]
prepend []     out = out
prepend (x:xs) out = x ++ prepend xs out

-- |Determine whether the given FileType is Directory.
isDirectoryFileType :: FileType -> Bool
isDirectoryFileType Directory = True
isDirectoryFileType _         = False

-- |Convert a filepath to a TypedFilePath. A Directory is a file
-- for which Directory.doesDirectoryExist returns true. If the path
-- does not represent a directory, it is considered a file, and there
-- is no further testing to verify that a file with that path actually
-- exists.
fileToTypedFilePath :: FilePath -> IO TypedFilePath
fileToTypedFilePath filepath =
  if isValid normPath
    then doesDirectoryExist filepath >>= \b ->
           case b of
             True   ->  return (Directory, normPath)
             False  ->  return (File, normPath)
    else ioError (userError $ "Invalid path: " ++ filepath)
  where normPath = dropTrailingPathSeparator filepath

-- |Sort the paths in a given directory by putting all directories first
-- and then sorting by path within each type. It is designed for sorting
-- all files and directories in the top level of a directory, and will
-- probably not provide a useful ordering if full paths are used.
sortPaths :: [TypedFilePath] -> [TypedFilePath]
sortPaths = sortBy compareTypedFilePaths

-- |Compare two typed file paths, using the built-in ordering of
-- FileType (dir before file) and path.
compareTypedFilePaths :: TypedFilePath -> TypedFilePath -> Ordering
compareTypedFilePaths (f1type, f1path) (f2type, f2path) =
  case typeComp of
    EQ          -> compare f1path f2path
    _           -> typeComp
  where typeComp = compare f1type f2type

-- |Normalize the filename of the given typed file path using the
-- supplied FilenameConverter function, which will be passed the
-- directory name (without parent directories) in case of a directory
-- or the filename (without any parent directories or the extension)
-- in case of a file. This function takes care of extracting the part
-- of the path that is to be normalized, calling the user-supplied
-- function with only that part, and then reassembling the
-- result of the filename converter into a full path again.
normalizeFilename :: FilenameConverter -> TypedFilePath -> String
normalizeFilename fn (fileType, origPath) =
  let (dir, filenameWithExt)   =  dirAndFile origPath
      (filenameNoExt, ext)     =  if fileType == Directory
                                     then (filenameWithExt, "")
                                     else fileAndExt filenameWithExt
      newFilenameNoExt = fn (fileType, filenameNoExt)
      result =  joinFileName (if dir == "./" then "" else dir) $
                    joinFileExt (if null newFilenameNoExt
                                 then filenameNoExt
                                 else newFilenameNoExt) (map toLower ext)
  in if null result then origPath else result

-- |The default filename converter, which normalizes a filename by
-- converting letters to lowercase and converting one or more undesirable
-- characters into a single hyphen (or removing altogether if at the
-- beginning or the end of the name). The only exception to these rules
-- is that an initial dot of a filename is not removed.
defaultFilenameConverter :: FilenameConverter
defaultFilenameConverter (_, path) = if isDotFile then ('.':result) else result
  where result = convert' Initial path
        isDotFile = not (null path) && head path == '.'

convert' :: State -> String -> String
convert' _      []      = []
convert' currState  (i:is)  =
  case (currState, nextState) of
    (HyphenBlock, NormalBlock) -> '-' : chr : rest
    (_          , NormalBlock) ->       chr : rest
    (_          , _          ) ->             rest
  where
    rest = convert' nextState is
    (emitted, nextState) = transition currState i
    chr = toLower $ maybe (error "FilenameNormalizer.normalize'") id emitted


-- |The state of finite state machine for normalizing.
data State =   Initial     -- initial state, until enter normal
             | HyphenBlock -- in block of chars to hyphenate
             | NormalBlock -- in block of normal chars to lowercase
  deriving (Eq, Show, Enum)

-- |The transition function of the fsm. The transition rules are
-- very simple: if we see a normal character, we always emit it
-- and transition to normal state. If we see a non-normal character,
-- we never emit it, and we stay in initial state if currently
-- in initial state, or else transition to hyphen state.
-- The normalize' function above takes care of emitting the
-- hyphen when we transition from hyphen to normal.
transition :: State -> Char -> (Maybe Char, State)
transition currState c
  | isAlphaNum c = (Just c, NormalBlock)
  | otherwise    = (Nothing, nonNormalState)
  where nonNormalState = case currState of
                           Initial   -> Initial
                           _         -> HyphenBlock

joinFileName :: String -> String -> String
joinFileName dirpath filename = joinPath [dirpath, filename]

joinFileExt :: String -> String -> String
joinFileExt filename ext = addExtension filename ext

-- |Split path into directory part and file part.
-- The directory part will not have an initial "./" or a trailing "/".
dirAndFile :: FilePath -> (String, String)
dirAndFile path = (normalizePath dirPath, fileName)
  where (dirPath, fileName) = splitFileName path

-- |Split file path into filename and ext.
fileAndExt :: FilePath -> (String, String)
fileAndExt filename =
  case splitExtension filename of
    ([],   ext) -> (ext, [])
    (file, ext) -> (file, ext)


normalizePath :: FilePath -> FilePath
normalizePath path = dropTrailingPathSeparator shortPath
  where shortPath = if take 2 path == "./" then drop 2 path else path
