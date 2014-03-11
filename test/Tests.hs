{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Maybe
import qualified Data.Vector as V
import           Test.Hspec
import           Test.HUnit

import Hemokit


_SERIAL :: SerialNumber
_SERIAL = fromJust $ makeSerialNumber "SN201211154288GM"


main :: IO ()
main = hspec $ do

  describe "decrypt" $ do

    it "decrypts 0123456789abcdef0123456789abcdef" $ do

      let b32      = "0123456789abcdef0123456789abcdef"
          expected = "\SUBp\176Ei\155%\183\237\145\185\166:\247eQ\SUBp\176Ei\155%\183\237\145\185\166:\247eQ"

      emotivRawDataBytes (decrypt _SERIAL Consumer b32) @?= expected


  describe "decrypting and parsing" $ do

    let encrypted = "S\a\205\165\195\182\244\DC4\NAKo\255K\\\247\146>tB\144q\165-\192\221\CANSa\150V,@\180"
        decrypted = decrypt _SERIAL Consumer encrypted
        rawData_expected = makeEmotivRawData "P}::\199\183\221\193|!\250h\205\NUL\NUL\NUL\STX\ENQX\r\162E|\218)\ETB\155\US\224gi9"
        packet_expected = EmotivPacket
          { packetCounter = 80
          , packetBattery = Nothing
          , packetGyroX   = -1
          , packetGyroY   = 8
          , packetSensors = V.fromList [8014,9132,7903,7617,7944,8102,9012,8277,8246,8773,7990,8849,7788,8160]
          , packetQuality = Just (FC6, 0)
          }

    it "decrypts a valid packet" $ decrypted             @?= rawData_expected
    it "parses a valid packet"   $ parsePacket decrypted @?= packet_expected
