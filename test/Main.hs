import           Test.Tasty (defaultMain, testGroup)

import           GCounter (gCounter)
import           GSet (gSet)
import           LWW (lww)
import           Max (maxTest)
import           PNCounter (pnCounter)

main :: IO ()
main = defaultMain $ testGroup "" [gCounter, gSet, lww, maxTest, pnCounter]
