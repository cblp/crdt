import           Test.Tasty (defaultMain, testGroup)

import           GCounter (gCounter)
import           GSet (gSet)
import           LWW (lww)
import           Max (maxTest)
import           MV (mv)
import           PNCounter (pnCounter)

main :: IO ()
main = defaultMain $ testGroup "" [gCounter, gSet, lww, maxTest, mv, pnCounter]
