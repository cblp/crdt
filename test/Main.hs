import           Test.Tasty (defaultMain, testGroup)

import           GCounter (gCounter)
import           LWW (lww)
import           PNCounter (pnCounter)
import           GSet      (gSet)

main :: IO ()
main = defaultMain $ testGroup "" [gCounter, gSet, lww, pnCounter]
