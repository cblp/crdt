import           Test.Tasty (defaultMain, testGroup)

import           Counter (counter)
import           GCounter (gCounter)
import           GSet (gSet)
import           LWW (lww)
import           Max (maxTest)
import           PNCounter (pnCounter)

main :: IO ()
main = defaultMain $
    testGroup "" [counter, gCounter, gSet, lww, maxTest, pnCounter]
