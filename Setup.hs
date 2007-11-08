import Distribution.Simple
import Distribution.PackageDescription (PackageDescription)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, buildDir)
import System.Exit
import Control.Monad
import System.Process

main :: IO ()
main = defaultMainWithHooks (defaultUserHooks { runTests = test })

test :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
test _ _ _ _ = runCommand commandStr >>= waitForProcess >> return ()
  where commandStr = "./quickcheck +names System/Denominate_Test.hs"
