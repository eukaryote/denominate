module TestRunner where

-- 	$Id: quickcheck,v 1.4 2003/01/08 15:09:22 shae Exp $	
-- This file defines a command
--      quickCheck <options> <files>
-- which invokes quickCheck on all properties defined in the files given as
-- arguments, by generating an input script for hugs and then invoking it.
-- quickCheck recognises the options
--      +names     print the name of each property before checking it
--      -names     do not print property names (the default)
-- Other options (beginning with + or -) are passed unchanged to hugs.
--
-- Change the first line of this file to the location of runhugs on your 
-- system.
-- Make the file executable.
--
-- TODO:
-- someone on #haskell asked about supporting QC tests inside LaTeX, ex. \{begin} \{end}, how?
-- add a verbosity switch that uses verboseCheck instead of quickCheck

import System
import Data.List

-- CS added for getting compiler version for base-? hackery
import System.Info
import Data.Version
import System.Process
import System.IO

main :: IO ()
main = do as<-getArgs
          sequence_ (map (process (filter isOption as)) 
	                 (filter (not.isOption) as))

-- ugly hack for .lhs files, is there a better way?
unlit [] = []
unlit x  = if (head x) == '>' then (tail x) else x

process opts file =
       let (namesOpt,opts') = getOption "names" "-names" opts in
       do xs<-readFile file
          let names = nub$ filter (\x -> (("> prop_" `isPrefixOf` x) || ("prop_" `isPrefixOf` x)))
	                (map (fst.head.lex.unlit) (lines xs)) 
          if null names then
	      putStr (file++": no properties to check\n")
	    else do baseStr <- ghcBaseVersion
                    writeFile "hugsin"$
	              unlines ((":l "++file):
	                       [(if namesOpt=="+names" then 
			           "putStr \""++p++": \" >> "
				 else "") ++
				"Test.QuickCheck.quickCheckWith (stdArgs { maxSuccess = 500, maxDiscard = 5000, maxSize = 100}) "++p | p<-names])
	            system ("ghci -v0 -package " ++ baseStr ++ " " ++ options opts' ++ " <hugsin")
	            return ()

isOption xs = head xs `elem` "-+"

options opts = unwords ["\""++opt++"\"" | opt<-opts]

getOption name def opts = 
  let opt = head [opt | opt<-opts++[def], isPrefixOf name (drop 1 opt)] in
    (opt, filter (/=opt) opts)


ghcBaseVersion = 
  do (stdin, stdout, stderr, phandle) <- runInteractiveProcess "ghc-pkg" ["--simple-output", "list", "base"] Nothing Nothing
     str <- hGetContents stdout -- returns a string like "base-3.0.3.0 base-4.0.0.0\n"
     -- we use any base-3 version available at present, and if none is available, 
     -- choose the first base in the list, which should be the earliest.
     let versions = words $ init str
     return $ maybe (head versions) id (findBase3Version versions)
  where
    findBase3Version []     = Nothing
    findBase3Version (v:vs) = if "base-3" `isPrefixOf` v
                                 then Just v
                                 else findBase3Version vs
