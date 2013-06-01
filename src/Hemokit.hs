{-# LANGUAGE NamedFieldPuns, DeriveDataTypeable #-}

module Hemokit
  ( decrypt
  , EegType (..)
  , EmotivException (..)
  , EmotivDeviceInfo (..)
  , EmotivDevice (..)
  , EmotivPacket (..)
  , Sensor (..)
  , _EMOTIV_VENDOR_ID
  , _EMOTIV_PRODUCT_ID
  , getEmotivDevices
  , primaryDevice
  , openEmotivDevice
  , readEmotivPacket
  ) where

import           Control.Applicative
import           Control.Exception
import           Crypto.Cipher.AES
import           Data.Bits ((.|.), (.&.), shiftL, shiftR)
import           Data.Char
import           Data.Data
import           Data.Ord (comparing)
import           Data.List
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Word
import           Data.ByteString as BS (ByteString, index)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified System.HIDAPI as HID
import           System.HIDAPI (DeviceInfo (..))


data EegType = Consumer | Developer deriving (Eq, Show)


newtype SerialNumber = SerialNumber ByteString -- must be 16 bytes

makeSerialNumber :: ByteString -> Maybe SerialNumber
makeSerialNumber b | BS.length b == 16 = Just $ SerialNumber b
                   | otherwise         = Nothing


-- Takes 32 bytes encrypted bytestring, returns 32 bytes decrypted bytestring.
decrypt :: SerialNumber -> EegType -> ByteString -> ByteString
decrypt (SerialNumber num) typ encrypted32bytes = BS.concat [decryptECB key left, decryptECB key right]
  where
    (left, right) = BS.splitAt 16 encrypted32bytes
    sn x | x >= 0    = index num x
         | otherwise = sn (BS.length num + x)
    c = fromIntegral . ord
    key = initKey . BS.pack $ start ++ middle ++ end

    start =        [ sn (-1), 0, sn (-2)]
    middle = case typ of
      Consumer ->  [ c 'T', sn (-3), 0x10, sn (-4), c 'B', sn (-1), 0   , sn (-2), c 'H']
      Developer -> [ c 'H', sn (-1), 0   , sn (-2), c 'T', sn (-3), 0x10, sn (-4), c 'B']
    end =          [ sn (-3), 0, sn (-4), c 'P']

data Sensor
  = F3
  | FC5
  | AF3
  | F7
  | T7
  | P7
  | O1
  | O2
  | P8
  | T8
  | F8
  | AF4
  | FC6
  | F4
  deriving (Eq, Enum, Bounded, Ord, Show)

allSensors :: [Sensor]
allSensors = [minBound .. maxBound]

newtype SensorMask = SensorMask [Word8] deriving (Eq, Show)

getSensorMask :: Sensor -> SensorMask
getSensorMask s = SensorMask $ case s of
  F3  -> [10, 11, 12, 13, 14, 15, 0, 1, 2, 3, 4, 5, 6, 7]
  FC5 -> [28, 29, 30, 31, 16, 17, 18, 19, 20, 21, 22, 23, 8, 9]
  AF3 -> [46, 47, 32, 33, 34, 35, 36, 37, 38, 39, 24, 25, 26, 27]
  F7  -> [48, 49, 50, 51, 52, 53, 54, 55, 40, 41, 42, 43, 44, 45]
  T7  -> [66, 67, 68, 69, 70, 71, 56, 57, 58, 59, 60, 61, 62, 63]
  P7  -> [84, 85, 86, 87, 72, 73, 74, 75, 76, 77, 78, 79, 64, 65]
  O1  -> [102, 103, 88, 89, 90, 91, 92, 93, 94, 95, 80, 81, 82, 83]
  O2  -> [140, 141, 142, 143, 128, 129, 130, 131, 132, 133, 134, 135, 120, 121]
  P8  -> [158, 159, 144, 145, 146, 147, 148, 149, 150, 151, 136, 137, 138, 139]
  T8  -> [160, 161, 162, 163, 164, 165, 166, 167, 152, 153, 154, 155, 156, 157]
  F8  -> [178, 179, 180, 181, 182, 183, 168, 169, 170, 171, 172, 173, 174, 175]
  AF4 -> [196, 197, 198, 199, 184, 185, 186, 187, 188, 189, 190, 191, 176, 177]
  FC6 -> [214, 215, 200, 201, 202, 203, 204, 205, 206, 207, 192, 193, 194, 195]
  F4  -> [216, 217, 218, 219, 220, 221, 222, 223, 208, 209, 210, 211, 212, 213]

qualityMask :: SensorMask
qualityMask = SensorMask [99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112]


getLevel :: ByteString -> SensorMask -> Int
getLevel decrypted32bytes (SensorMask sensorBits) = fromIntegral $ foldl' f 0 sensorBits
  where
    f :: Word8 -> Word8 -> Word8
    f level bit = (level `shiftL` 1) .|. ((decrypted32bytes `index` fromIntegral b) `shiftR` fromIntegral o .&. 1)
      where
        b = (bit `shiftR` 3) + 1 :: Word8
        o = bit .&. 7            :: Word8 -- mod 8


-- TODO this might have to be adjusted
batteryValue :: Word8 -> Int
batteryValue batteryByte = case batteryByte of
  b | b >= 248 -> 100
  247          -> 99
  246          -> 97
  245          -> 93
  244          -> 89
  243          -> 85
  242          -> 82
  241          -> 77
  240          -> 72
  239          -> 66
  238          -> 62
  237          -> 55
  236          -> 46
  235          -> 32
  234          -> 20
  233          -> 12
  232          -> 6
  231          -> 4
  230          -> 3
  229          -> 2
  228          -> 1
  227          -> 1
  226          -> 1
  _            -> 0


-- | Which sensor's quality is transmitted in the packet (depends on first byte).
qualitySensorFromByte0 :: Word8 -> Maybe Sensor
qualitySensorFromByte0 packetNo = case packetNo of
  0  -> Just F3
  1  -> Just FC5
  2  -> Just AF3
  3  -> Just F7
  4  -> Just T7
  5  -> Just P7
  6  -> Just O1
  7  -> Just O2
  8  -> Just P8
  9  -> Just T8
  10 -> Just F8
  11 -> Just AF4
  12 -> Just FC6
  13 -> Just F4
  14 -> Just F8
  15 -> Just AF4
  64 -> Just F3
  65 -> Just FC5
  66 -> Just AF3
  67 -> Just F7
  68 -> Just T7
  69 -> Just P7
  70 -> Just O1
  71 -> Just O2
  72 -> Just P8
  73 -> Just T8
  74 -> Just F8
  75 -> Just AF4
  76 -> Just FC6
  77 -> Just F4
  78 -> Just F8
  79 -> Just AF4
  80 -> Just FC6
  _  -> Nothing
  -- TODO check why FC6 is the only one that appears 3 times.



data EmotivPacket = EmotivPacket
  { counter   :: Int
  , battery   :: Maybe Int
  , gyroX     :: Int -- ^ turning "left" gives positive numbers
  , gyroY     :: Int -- ^ turning "down" gives positive numbers
  , sensors   :: Vector Int
  , quality   :: Maybe ( Sensor, Int )
  } deriving (Eq, Show)


-- I improved the gyro like this:
-- https://github.com/openyou/emokit/commit/b023a3c195410147dae44a3ce3a6d72f7c16e441
-- TODO check by graphing if that is really correct vs the old implementation.

parsePacket :: ByteString -> EmotivPacket
parsePacket decrypted32bytes = EmotivPacket
    { counter   = if is128c then 128                else fromIntegral byte0
    , battery   = if is128c then Just (batteryValue byte0) else Nothing
    , gyroX     = ((int (byte 29) `shiftL` 4) .|. int (byte 31 `shiftR` 4)) - 1652
    , gyroY     = ((int (byte 30) `shiftL` 4) .|. int (byte 31   .&. 0x0F)) - 1681
    , sensors   = V.fromList [ getLevel decrypted32bytes (getSensorMask s) | s <- allSensors ]
    , quality   = do
        s <- qualitySensorFromByte0 byte0
        let l = getLevel decrypted32bytes qualityMask
        return ( s, l )
    }
  where
    int n  = fromIntegral n :: Int
    byte0  = byte 0
    byte n = decrypted32bytes `index` n
    is128c = byte0 .&. 128 /= 0 -- is it the packet which would be sequence no 128?

_EMOTIV_VENDOR_ID, _EMOTIV_PRODUCT_ID :: Word16
_EMOTIV_VENDOR_ID = 8609
_EMOTIV_PRODUCT_ID = 1

data EmotivException = InvalidSerialNumber String
                     | CouldNotReadSerial String -- ^ with path to the device
                     | OtherEmotivException String
                     deriving (Data, Typeable)

instance Exception EmotivException

instance Show EmotivException where
  show (InvalidSerialNumber sn)   = "Emotiv ERROR: the device serial number " ++ sn ++ " does not look valid"
  show (CouldNotReadSerial path)  = "Emotiv ERROR: could not read serial number of device " ++ path ++ ". Maybe you are not running as root?"
  show (OtherEmotivException err) = "Emotiv ERROR: " ++ err


data EmotivDeviceInfo = EmotivDeviceInfo
  { hidapiDeviceInfo :: DeviceInfo
  } deriving (Show)

data EmotivDevice = EmotivDevice
  { hidapiDevice  :: HID.Device
  , serial        :: SerialNumber
  }

getEmotivDevices :: IO [EmotivDeviceInfo]
getEmotivDevices = map EmotivDeviceInfo <$> HID.enumerate (Just _EMOTIV_VENDOR_ID) (Just _EMOTIV_PRODUCT_ID)


primaryDevice :: [EmotivDeviceInfo] -> EmotivDeviceInfo
primaryDevice = maximumBy (comparing (interfaceNumber . hidapiDeviceInfo))


openEmotivDevice :: EmotivDeviceInfo -> IO EmotivDevice
openEmotivDevice EmotivDeviceInfo{ hidapiDeviceInfo } = case hidapiDeviceInfo of
  d@DeviceInfo{ serialNumber = Just sn } ->
    case makeSerialNumber (BS8.pack sn) of
      Nothing -> throwIO $ InvalidSerialNumber sn
      Just s  -> do 
        hidDev <- HID.openDeviceInfo d
        return EmotivDevice
          { hidapiDevice  = hidDev
          , serial        = s
          }
  DeviceInfo{ path } -> throwIO $ CouldNotReadSerial path


readEmotivPacket :: EmotivDevice -> IO EmotivPacket
readEmotivPacket EmotivDevice{ hidapiDevice, serial } = do
  d32 <- HID.read hidapiDevice 32
  let decrypted = decrypt serial Consumer d32
  return $ parsePacket decrypted
