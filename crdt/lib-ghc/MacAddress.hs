module MacAddress where

import           Data.Binary (decode)
import qualified Data.ByteString.Lazy as BSL
import           Data.Word (Word64)
import           Network.Info (MAC (MAC), getNetworkInterfaces, mac)
import           Safe (headDef)

getMacAddress :: IO Word64
getMacAddress = decodeMac <$> getMac

getMac :: IO MAC
getMac =
    headDef (error "Can't get any non-zero MAC address of this machine")
        .   filter (/= minBound)
        .   map mac
        <$> getNetworkInterfaces

decodeMac :: MAC -> Word64
decodeMac (MAC b5 b4 b3 b2 b1 b0) =
    decode $ BSL.pack [0, 0, b5, b4, b3, b2, b1, b0]
