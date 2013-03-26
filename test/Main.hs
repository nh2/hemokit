module Main (main) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           Test.Hspec

import Hemokit


_SERIAL :: ByteString
_SERIAL = BS8.pack "SN201211154288GM"


main :: IO ()
main = hspec $ do
  describe "decrypt" $ do

    it "decrypts 0123456789abcdef" $
      decrypt _SERIAL Consumer b32 == BS8.pack "\SUBp\176Ei\155%\183\237\145\185\166:\247eQ"
        where
          b32 = BS8.pack "0123456789abcdef"


