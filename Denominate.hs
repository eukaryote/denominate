{- |
Module      :  $Header$
Description :  Supports bulk renaming of directories and files into a
               standard, normalized format.
Copyright   :  (c) Calvin Smith
License     :  BSD3, see LICENSE.txt

Maintainer  :  Calvin Smith <cs-haskell@protempore.net>
Stability   :  Experimental
Portability :  portable

Functions to assist in renaming of directories and files into a
standard, normalized format.

This module defines several functions supporting renaming of files
and directories, and is especially useful for doing a bulk renaming
of all files and directories, recursively, in a given base directory.

The primary functions of interest are 'rename' and 'renameAll', both
of which accept a function for creating the new filename based on its
current name. The user may supply a custom filename converter function,
or may use the pre-defined function that this module defines.

The standard pre-defined converter determines the new name for a file
or directory using the following rules:

  1. all letters are converted to lowercase;

  2. all non-alphanumeric characters at the beginning of a file or
     directory name are removed (with the exception of an initial '.');

  3. all non-alphanumeric characters at the end of a directory name
     or the end of a filename (before the extension) are removed;

  4. all other blocks of one or more non-alphanumeric characters are
     converted to a single hyphen.

See the documentation of the exported functions for more information.
-}

module Denominate (FileType(Directory, File),
                   RenameResult(Success, Failure),
                   TypedFilePath, FilenameConverter,
                   normalizeFilename, allFilepaths, rename, renameAll,
                   fileToTypedFilePath, defaultFilenameConverter)
where
import Internal
