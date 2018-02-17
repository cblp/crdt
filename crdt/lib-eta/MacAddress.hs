module MacAddress where

import           Java

import           Data.Binary (decode)
import qualified Data.ByteString.Lazy as BSL
import           Data.Maybe (catMaybes)
import           Data.Traversable (for)
import           Data.Word (Word64, Word8)
import           Safe (headDef)

data NetworkInterface = NetworkInterface @java.net.NetworkInterface
    deriving Class

foreign import java unsafe
    "@static java.net.NetworkInterface.getNetworkInterfaces"
    getNetworkInterfaces :: Java a (Enumeration NetworkInterface)

foreign import java unsafe
    getHardwareAddress :: Java NetworkInterface (Maybe JByteArray)

getMacAddress :: IO Word64
getMacAddress = java $ do
    interfaces <- fromJava <$> getNetworkInterfaces
    macs <- for interfaces (<.> getHardwareAddress)
    let macBytes =
            headDef (error "Can't get any non-zero MAC address of this machine")
            $ catMaybes
            $ macs
    let mac = foldBytes $ fromJava macBytes
    pure mac

foldBytes :: [Word8] -> Word64
foldBytes bytes = decode . BSL.pack $ replicate (8 - length bytes) 0 ++ bytes
