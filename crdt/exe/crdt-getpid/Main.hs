import           CRDT.LamportClock (getMacAddress)

main :: IO ()
main = print =<< getMacAddress
