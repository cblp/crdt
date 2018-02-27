{-# LANGUAGE CPP #-}

module MacAddress where

import           Data.Binary (decode)
import qualified Data.ByteString.Lazy as BSL
import           Safe (headDef)

#ifdef ETA_VERSION

import           Java

import           Data.Maybe (catMaybes)
import           Data.Traversable (for)
import           Data.Word (Word64, Word8)

#else /* !defined ETA_VERSION */

import           Data.Word (Word64)
import           Network.Info (MAC (MAC), getNetworkInterfaces, mac)

#endif /* ETA_VERSION */

getMacAddress :: IO Word64

#ifdef ETA_VERSION

getMacAddress = java $ do
    interfaces <- fromJava <$> getNetworkInterfaces
    macs <- for interfaces (<.> getHardwareAddress)
    let macBytes =
            headDef (error "Can't get any non-zero MAC address of this machine")
            $ catMaybes macs
    let mac = foldBytes $ fromJava macBytes
    pure mac

data NetworkInterface = NetworkInterface @java.net.NetworkInterface
    deriving Class

foreign import java unsafe
    "@static java.net.NetworkInterface.getNetworkInterfaces"
    getNetworkInterfaces :: Java a (Enumeration NetworkInterface)

foreign import java unsafe
    getHardwareAddress :: Java NetworkInterface (Maybe JByteArray)

foldBytes :: [Word8] -> Word64
foldBytes bytes = decode . BSL.pack $ replicate (8 - length bytes) 0 ++ bytes

#else /* !defined ETA_VERSION */

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

#endif /* ETA_VERSION */
