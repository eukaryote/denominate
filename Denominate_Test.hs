{- OPTIONS_GHC -fallow-overlapping-instances -}

import Denominate
import Data.List(intersperse)
import Data.Char
import Test.QuickCheck


instance Arbitrary FileType where
  arbitrary = oneof $ map return [Directory, File]
  coarbitrary = variant . fromEnum

randPathGen = 
    do numSegments <- choose (0, 8)
       words <- mapM (\_ -> choose (0, 10) >>= randWordGen) [0..numSegments]
       return $ concat $ intersperse "/" words

randWordGen len = mapM (\i -> randCharGen) [0..len]

randCharGen = oneof $ map return chars
 where chars = letters ++ map toUpper letters ++ punc
       letters = "abcdefghijklmnopqrstuvwxyz"
       punc    = "..............-----_____,?`~!@#$%^&*()=+[]{}|'\"<>?/"

lastSlashOf = lastIndexOf '/'
lastPeriodOf = lastIndexOf '.'

lastPathPart path = if n > -1 then drop (n+1) path else path
  where n = lastSlashOf path

lastIndexOf chr str = lastIndexOf' str 0 (-1)
  where
    lastIndexOf' []     i highest = highest
    lastIndexOf' (c:cs) i highest =
      case c == chr of
        True  -> lastIndexOf' cs (i+1) i
        False -> lastIndexOf' cs (i+1) highest

convert = normalizeFilename defaultFilenameConverter

hasLetter = any isLetter

hasLegalChar = any (\c -> isLetter c || c == '-')

lastDirPartHasLetter path = hasLetter $ drop n path
  where n = lastSlashOf path

lastPathPartHasExt path = not $ null (ext filename)
  where n = lastSlashOf path
        filename = drop n path
        n' = lastPeriodOf filename

hasInitialDot p = char1 p == "."
char1 = take 1
isInitialGarbageChar c = not (isLetter c || c == '.')
hasInitialGarbageChar s | null s    = False
                        | otherwise = isInitialGarbageChar $ head s
hasTrailingGarbageChar s | null s    = False
                         | otherwise = not $ isLetter $ last s

stripExt fname = 
  case n > 0 of
    True   ->  take n fname
    False  ->  fname
  where n = lastPeriodOf fname

ext fname =
  case lastDotIndex < 1 of
    True  ->  ""
    False ->  drop (lastDotIndex + 1) fname
  where
    lastDotIndex = lastIndexOf '.' fname

-- PROPERTIES:

-- Only the filename or the very last directory name (everything before
-- the last slash) should ever change.
prop_changesOnlyLastPart :: FileType -> Property
prop_changesOnlyLastPart ftype = 
  forAll randPathGen test
  where
    test path = take n path == take n result
      where n = lastSlashOf path + 1
            result = convert (ftype, path)


-- every char in last part of a directory will be either a lowercase letter
-- or a hyphen if there is at least one letter in the last part of the 
-- original path, with the possible exception of an initial dot
prop_dirLastPartLegalChars :: Property
prop_dirLastPartLegalChars =
  forAll randPathGen f
  where
    f p = lastDirPartHasLetter p ==> test p
    test path = let dirResult = lastPathPart $ result path
                    initChar = head dirResult
                in  if not (null dirResult)
                       then initChar == '.' || initChar == '-' || isLower initChar &&
                              all (\c -> c == '-' || isLower c) (tail dirResult)
                       else True
    result p = convert (Directory, p)
                
-- if the original filename without extension has at least one letter,
-- then the new filename without extension should consist of nothing but
-- lowercase letters and hyphens.
prop_fileLastPartLegalChars :: Property
prop_fileLastPartLegalChars =
  forAll randPathGen (\p -> hasLetter (extractFilename p) ==> test p)
  where
    test path =  all (\c -> isLetter c || c == '.' || c == '-') (newFileNoExt path)
    newFileNoExt p = extractFilename $ convert (File, p)
    extractFilename = stripExt . lastPathPart 

-- the extension of a file should only be lowercased, with no other
-- changes made.
prop_fileExtOnlyLowercased :: Property
prop_fileExtOnlyLowercased =
  forAll randPathGen (\p -> lastPathPartHasExt p ==> test p)
  where
    test path = f result == map toLower (f path)
      where result = convert (File, path)
            f = ext . lastPathPart

-- a file that begins with a '.' should not have the '.' removed
prop_fileInitialDotUnchanged :: Property
prop_fileInitialDotUnchanged =
  forAll randPathGen test
  where
    test p =  hasInitialDot (lastPathPart p) ==>  
                   hasInitialDot (lastPathPart $ convert (File, p))

-- the length of the converted path is never longer than the original path
prop_pathNeverLongerAfterConvert :: FileType -> Property
prop_pathNeverLongerAfterConvert ft =
  forAll randPathGen (\p -> length (convert (ft, p)) <= length p)

-- there should always be the same number of letters in the path
-- before and after, because letters are never removed.
prop_numLettersBeforeAndAfterAreEqual :: FileType -> Property
prop_numLettersBeforeAndAfterAreEqual ft =
  forAll randPathGen (\p -> f (convert (ft, p)) == f p)
  where
    f p = length $ filter isLetter p

-- the letters in the original should be equal to, and in the
-- same order as, the letters in the converted path, except
-- for case differences.
prop_lettersBeforeAndAfterAreEqual :: FileType -> Property
prop_lettersBeforeAndAfterAreEqual ft =
  forAll randPathGen (\p -> f (convert (ft, p)) == f p)
  where
    f p = map toLower $ filter isLetter p

-- if the file has at least one letter in the filename without extension,
-- then the first character of the converted filename will be a non-garbage
-- character (letter or '.').
prop_initialFileGarbageIsRemoved :: Property
prop_initialFileGarbageIsRemoved =
  forAll randPathGen test
  where
    test p = hasLetter origLastPathPartNoExt ==> 
               not (hasInitialGarbageChar newLastPathPartNoExt)
      where
        origLastPathPartNoExt = stripExt $ lastPathPart p
        newLastPathPartNoExt  = stripExt $ lastPathPart $ convert (File, p)

-- likewise for directory, but we don't consider extensions at all
prop_initialDirGarbageIsRemoved :: Property
prop_initialDirGarbageIsRemoved =
  forAll randPathGen test
  where
    test p = hasLetter origLastPathPart ==>
               not (hasInitialGarbageChar newLastPathPart)
      where
        origLastPathPart = lastPathPart p
        newLastPathPart  = lastPathPart $ convert (Directory, p)


-- if the file has at least one letter in the filename without extension,
-- then the last character of the converted filename will be a non-garbage
-- character (letter or '.').
prop_trailingFileGarbageIsRemoved :: Property
prop_trailingFileGarbageIsRemoved =
  forAll randPathGen test
  where 
    test p = hasLetter origLastPathPartNoExt ==>
               not (hasTrailingGarbageChar newLastPathPartNoExt)
      where
        origLastPathPartNoExt = stripExt $ lastPathPart p
        newLastPathPartNoExt  = stripExt $ lastPathPart $ convert (File, p)

-- likewise for directory, but we don't consider extensions at all
prop_trailingDirGarbageIsRemoved :: Property
prop_trailingDirGarbageIsRemoved =
  forAll randPathGen test
  where 
    test p = hasLetter origLastPathPart ==>
               not (hasTrailingGarbageChar newLastPathPart)
      where
        origLastPathPart = lastPathPart p
        newLastPathPart  = lastPathPart $ convert (Directory, p)