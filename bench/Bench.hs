{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Criterion.Main
import           Data.Maybe

import Hemokit


main :: IO ()
main = defaultMain
  [ bench "parsePacket" $ nf parsePacket rawPacket1
  , bench "decrypt"     $ nf (decrypt _SERIAL Consumer) encryptedPacket1
  ]
  where
    _SERIAL = fromJust $ makeSerialNumber "SN201211154288GM"

    rawPacket1 = makeEmotivRawData "|\147~E\ETB\184\155\230y\185\247\249\&1@\NUL\NUL\STX\NUL\184\ACK\158ms\221\246\183\156\RSOgir"

    encryptedPacket1 = "\170\182\217\GS \170\NULR\237\130\&7\191\227}K#\228\153Ew\188_\228\129\232\186K\246\135\DEL\129z"
