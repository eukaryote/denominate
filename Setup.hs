import Distribution.Simple
import Distribution.PackageDescription (PackageDescription)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, buildDir)
import System.Exit
import Control.Monad
import System.Process


main = defaultMainWithHooks defaultUserHooks { runTests = test }

test :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ExitCode
test _ _ _ _ = waitForProcess =<< runCommand commandStr
  where commandStr = "./quickcheck +names Denominate_Test.hs"
