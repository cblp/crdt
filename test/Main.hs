import           Test.Tasty (defaultMain, testGroup)

import qualified Cm.TwoPSet as Cm
import           Counter (counter)
import qualified Cv.TwoPSet as Cv
import           GCounter (gCounter)
import           GSet (gSet)
import           LWW (lww)
import           Max (maxTest)
import           PNCounter (pnCounter)

main :: IO ()
main = defaultMain $ testGroup ""
    [counter, gCounter, gSet, lww, maxTest, pnCounter, Cv.twoPSet, Cm.twoPSet]
