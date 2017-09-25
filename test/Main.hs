import           Test.Tasty (defaultMain, testGroup)

import           GCounter (gCounter)
import           LWW (lww)
import           PNCounter (pnCounter)

main :: IO ()
main = defaultMain $ testGroup "" [gCounter, lww, pnCounter]
