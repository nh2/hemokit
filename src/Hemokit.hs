module Hemokit
( decrypt
, EegType (..)
) where

import           Crypto.Cipher.AES
import           Data.Char
import           Data.ByteString as BS (ByteString, index)
import qualified Data.ByteString as BS


data EegType = Consumer | Developer deriving (Eq, Show)

type SerialNumber = ByteString



decrypt :: SerialNumber -> EegType -> ByteString -> ByteString
decrypt num typ encrypted = BS.concat [decryptECB key left, decryptECB key right]
  where
    (left, right) = BS.splitAt 16 encrypted
    sn x | x >= 0    = index num x
         | otherwise = sn (BS.length num + x)
    c = fromIntegral . ord
    key = initKey . BS.pack $ start ++ middle ++ end

    start =                     [ sn (-1)
                                , 0
                                , sn (-2)]
    middle = case typ of Consumer ->  [ c 'T'
                                      , sn (-3)
                                      , 0x10
                                      , sn (-4)
                                      , c 'B'
                                      , sn (-1)
                                      , 0
                                      , sn (-2)
                                      , c 'H']
                         Developer -> [ c 'H'
                                      , sn (-1)
                                      , 0
                                      , sn (-2)
                                      , c 'T'
                                      , sn (-3)
                                      , 0x10
                                      , sn (-4)
                                      , c 'B']
    end =                       [ sn (-3)
                                , 0
                                , sn (-4)
                                , c 'P']
