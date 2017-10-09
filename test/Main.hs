import           Test.Tasty (defaultMain, testGroup)

import           Counter (counter)
import           GCounter (gCounter)
import           GSet (gSet)
import           LWW (lww)
import           Max (maxTest)
import           PNCounter (pnCounter)
import qualified TPSet as Cm
import qualified Cv.TPSet as Cv

main :: IO ()
main = defaultMain $
    testGroup "" [counter, gCounter, gSet, lww, maxTest, pnCounter, Cv.tpSet, Cm.tpSet]
