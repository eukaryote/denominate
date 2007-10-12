{- OPTIONS_GHC -fallow-overlapping-instances -}

import Denominate
import Data.List
import Control.Monad
import Data.Char(toUpper,isLetter)
import Test.QuickCheck

run_all tests = mapM_ (\f -> putStrLn ("FAIL: " ++ show f)) failures
  where failures = filter (\(_, (r, b)) -> not b) $ test_all test_cases

test_all = map (\f -> (f, test_single f))

test_single (in_file, expected) = (result, result == expected)
  where result = normalizeFilename defaultFilenameConverter in_file


-- some known files to be tested manually using above run_all func.
test_cases = [
  ((File, "."),              "."),
  ((Directory, "."),         "."),
  ((File, "a"),              "a"),
  ((Directory, "a"),         "a"),
  ((File, "A"),              "a"),
  ((Directory, "A"),         "a"),
  ((File, "_a.TXT"),         "a.txt"),
  ((Directory, "_a.DIR"),    "a-dir"),
  ((File, "a-.b"),           "a.b"),
  ((Directory, "a-.b"),      "a-b"),
  ((File, "TEST"),           "test"),
  ((Directory, "TEST"),      "test"),
  ((File, "_%$.a.bar.out"),  "a-bar.out"),
  ((Directory, "_%$.a.bar.out"), "a-bar-out"),
  ((File, "hello.txt"),      "hello.txt"),
  ((Directory, "hello.txt"), "hello-txt"),
  ((File, "./T*EST.C"),      "./t-est.c"),
  ((Directory, "./T*EST.C"), "./t-est-c"),
  ((File, "T*&#$(EST.Out"),  "t-est.out"),
  ((Directory, "T*&#EST.Out"), "t-est-out"),
  ((File, "../foo.BAR"),     "../foo.bar"),
  ((Directory, "../Foo-*baR"), "../foo-bar"),
  ((File, "Dir/_test_file.bar.TXT"), "Dir/test-file-bar.txt"),
  ((Directory, "Dir/_test_dIR"), "Dir/test-dir"), -- only last part is changed
  ((File, "~!#dir/_test__file._"), "~!#dir/test-file._"),
  ((Directory, "@!*&%$dir/_Test_DIR._"), "@!*&%$dir/test-dir"),
  ((File, "../Foo/BAR/asDF_.txt"), "../Foo/BAR/asdf.txt"),
  ((Directory, "../Foo/BAR/asDF_"), "../Foo/BAR/asdf")
 ]

instance Arbitrary FileType where
  arbitrary = oneof $ map return [Directory, File]

rand_path_gen = 
    do numSegments <- choose (0, 8)
       words <- mapM (\_ -> choose (0, 10) >>= rand_word_gen) [0..numSegments]
       return $ concat $ intersperse "/" words

rand_word_gen len = mapM (\i -> rand_char_gen) [0..len]

rand_char_gen = oneof $ map return chars
 where chars = letters ++ map toUpper letters ++ punc
       letters = "abcdefghijklmnopqrstuvwxyz"
       punc    = "..............-----_____,?`~!@#$%^&*()=+[]{}|'\"<>?/"

last_slash str = last_slash' str 0 (-1)
  where
    last_slash' [] i highest = highest
    last_slash' (c:cs) i highest = 
      case c == '/' of
        True   -> last_slash' cs (i+1) i
        False  -> last_slash' cs (i+1) highest

convert = normalizeFilename defaultFilenameConverter

has_letter = any isLetter

last_part_has_letter path = has_letter $ drop n path
  where n = last_slash path

-- Only the filename or the very last directory name (everything before
-- the last slash) should ever change.
prop_changes_only_last_part :: FileType -> Property
prop_changes_only_last_part ftype = 
  forAll rand_path_gen test
  where
    test path = take n path == take n result
      where n = last_slash path + 1
            result = convert (ftype, path)


-- every char in last part of a directory will be either a lowercase letter
-- or a hyphen if there is at least one letter in the last part of the 
-- original path
prop_dir_last_part_legal_chars :: Property
prop_dir_last_part_legal_chars =
  forAll rand_path_gen (\p -> last_part_has_letter p ==> test p)
  where
    test path = all (\c -> c == '-' || (c >= 'a' && c <= 'z')) (drop n res)
      where n = last_slash path + 1
            res = convert (Directory, path)

