{-# LANGUAGE NamedFieldPuns, TupleSections, DeriveDataTypeable, DeriveGeneric #-}

-- | A library for reading from an Emotic EPOC EEG.
--
-- * Use `getEmotivDevices` to list available EEGs.
--
-- * Use `openEmotivDevice` to open a device for reading.
--
-- * Use `readEmotiv` read from an open device.
--
-- * You will obtain `EmotivPacket`s and `EmotivState`s.
module Hemokit
  ( -- * Opening and reading from EEGs
    _EMOTIV_VENDOR_ID
  , _EMOTIV_PRODUCT_ID
  , EmotivDeviceInfo (..)
  , EmotivRawDevice (..)
  , EmotivDevice (..)
  , getEmotivDevices
  , openEmotivDevice
  , openEmotivDeviceFile
  , readEmotiv
  , EmotivException (..)
  , SerialNumber ()
  , makeSerialNumber
  , makeSerialNumberFromString
  , deviceInfoSerial

  -- EEG models
  , EmotivModel (..)

  -- * EEG data
  , EmotivPacket (..)
  , EmotivState (..)
  , Sensor (..)
  , allSensors

  -- * Dealing with (decrypted) raw data
  , EmotivRawData (..)
  , readEmotivRaw
  , makeEmotivRawData
  , parsePacket
  , updateEmotivState

  -- * Encrypted raw data
  , decrypt

  -- * Internals
  , BitMask (..)
  , getSensorMask
  , qualityMask
  , getLevel
  , batteryValue
  , qualitySensorFromByte0

  -- * Interactive use
  , withDataFromLastEEG
  ) where

import           Control.Applicative
import           Control.DeepSeq.Generics
import           Control.Exception
import           Control.Monad
import           Crypto.Cipher.AES
import           Data.Bits ((.|.), (.&.), shiftL, shiftR)
import           Data.Char
import           Data.Data
import           Data.IORef
import           Data.List
import           Data.Ord (comparing)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Word
import           Data.ByteString as BS (ByteString, index)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           GHC.Generics (Generic)
import qualified System.HIDAPI as HID
import           System.HIDAPI (DeviceInfo (..))
import           System.IO


-- | Whether the EPOC is a consumer or developer model.
--
-- This affects how the EEG data is to be decrypted.
--
-- You can check if you are using the correct model by seeing if the packet
-- `counter` increases from 0 until 128 on subsequent packets.
data EmotivModel = Consumer | Developer deriving (Eq, Show, Generic)


-- | A valid Emotiv serial number. 16 bytes.
newtype SerialNumber = SerialNumber ByteString deriving (Eq, Show, Generic)

-- | Checks an Emotiv serial, returning a `SerialNumber` if it's valid.
makeSerialNumber :: ByteString -> Maybe SerialNumber
makeSerialNumber b | BS.length b == 16 = Just $ SerialNumber b
                   | otherwise         = Nothing

-- | Like `makeSerialNumber`, using a `String`.
makeSerialNumberFromString :: String -> Maybe SerialNumber
makeSerialNumberFromString = makeSerialNumber . BS8.pack


-- | Takes a 32 bytes encrypted EEG data, returns 32 bytes decrypted EEG data.
decrypt :: SerialNumber -> EmotivModel -> ByteString -> EmotivRawData
decrypt (SerialNumber num) typ encrypted32bytes = makeEmotivRawData decrypted32bytes
  where
    decrypted32bytes = BS.concat [decryptECB key left, decryptECB key right]

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

-- | The sensors of an Emotiv EPOC.
-- Uses the names from the International 10-20 system.
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
  deriving (Eq, Enum, Bounded, Ord, Show, Generic)

-- | Contains all `Sensor`s.
allSensors :: [Sensor]
allSensors = [minBound .. maxBound]


-- | Describes the indices of bits to make up a certain value.
newtype BitMask = BitMask [Word8] deriving (Eq, Show)

-- | Describes which bits in a raw data packet make up the given sensor.
getSensorMask :: Sensor -> BitMask
getSensorMask s = BitMask $ case s of
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

-- | Describes which bits in a raw data packat make up a sensor quality value.
qualityMask :: BitMask
qualityMask = BitMask [99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112]


-- | Extracts the sensor value for the given sensor from Emotiv raw data.
getLevel :: EmotivRawData -> BitMask -> Int
getLevel (EmotivRawData bytes32) (BitMask sensorBits) = foldr f 0 sensorBits
  where
    f :: Word8 -> Int -> Int
    f bitNo level = (level `shiftL` 1) .|. int (bitAt b o)
      where
        b = (bitNo `shiftR` 3) + 1 :: Word8 -- div by 8 to get byte number, skip first byte (counter)
        o = bitNo .&. 7            :: Word8 -- mod by 8 to get bit offset

    bitAt :: Word8 -> Word8 -> Word8
    bitAt byte bitOffset = ((bytes32 `index` int byte) `shiftR` int bitOffset) .&. 1


-- | `fromIntegral` shortcut.
int :: (Integral a) => a -> Int
int = fromIntegral


-- TODO this might have to be adjusted
-- | Parses a battery percentage value from a byte.
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


-- | Which sensor's quality is transmitted in the packet
-- (depends on first byte, the packet counter).
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



-- | Contains the data of a single packet sent from the device.
-- Accumulated data (the current state) is available in `EmotivState`.
data EmotivPacket = EmotivPacket
  { packetCounter :: Int                 -- ^ counts up from 0 to 127 (128 Hz)
  , packetBattery :: Maybe Int           -- ^ the current battery percentage
  , packetGyroX   :: Int                 -- ^ turning "left" gives positive numbers
  , packetGyroY   :: Int                 -- ^ turning "down" gives positive numbers
  , packetSensors :: Vector Int          -- ^ EEG sensor values
  , packetQuality :: Maybe (Sensor, Int) -- ^ EEG sensor-to-skin connectivity
  } deriving (Eq, Show, Generic)


-- | Contains the "current state" of the EEG, cumulateively updated by
-- incoming `EmotivPacket`s.
data EmotivState = EmotivState
  { counter   :: Int        -- ^ counts up from 0 to 127 (128 Hz)
  , battery   :: Int        -- ^ the current battery percentage
  , gyroX     :: Int        -- ^ turning "left" gives positive numbers
  , gyroY     :: Int        -- ^ turning "down" gives positive numbers
  , sensors   :: Vector Int -- ^ EEG sensor values
  , qualities :: Vector Int -- ^ EEG sensor-to-skin connectivity
  } deriving (Eq, Show, Generic)


-- | Wraps (unencrypted) Emotiv raw data. Ensures that it is 32 bytes.
newtype EmotivRawData = EmotivRawData
  { emotivRawDataBytes :: ByteString
  } deriving (Eq, Show, Generic)


-- | Treat a `ByteString` as Emotiv raw data.
-- Errors if the input is non 32 bytes.
makeEmotivRawData :: ByteString -> EmotivRawData
makeEmotivRawData bytes
  | BS.length bytes /= 32 = error "Emotiv raw data must be 32 bytes"
  | otherwise             = EmotivRawData bytes


-- I improved the gyro like this:
-- https://github.com/openyou/emokit/commit/b023a3c195410147dae44a3ce3a6d72f7c16e441

-- | Parses an `EmotivPacket` from raw bytes.
parsePacket :: EmotivRawData -> EmotivPacket
parsePacket raw@(EmotivRawData bytes32) = EmotivPacket
  { packetCounter = if is128c then 128                       else fromIntegral byte0
  , packetBattery = if is128c then Just (batteryValue byte0) else Nothing
  , packetGyroX   = ((int (byte 29) `shiftL` 4) .|. int (byte 31 `shiftR` 4)) - 1652 -- TODO check this hardcoding
  , packetGyroY   = ((int (byte 30) `shiftL` 4) .|. int (byte 31   .&. 0x0F)) - 1681
  , packetSensors = V.fromList [ getLevel raw (getSensorMask s) | s <- allSensors ]
  , packetQuality = (, getLevel raw qualityMask) <$> qualitySensorFromByte0 byte0
  }
  where
    byte0  = byte 0
    byte n = bytes32 `index` n
    is128c = byte0 .&. 128 /= 0 -- Is it the packet which would be sequence no 128?
                                -- If so, then byte0 is the battery value.


-- | The USB vendor ID of the Emotiv EPOC.
_EMOTIV_VENDOR_ID :: HID.VendorID
_EMOTIV_VENDOR_ID = 8609

-- | The USB product ID of the Emotiv EPOC.
_EMOTIV_PRODUCT_ID :: HID.ProductID
_EMOTIV_PRODUCT_ID = 1


-- | Emotiv related errors.
data EmotivException
  = InvalidSerialNumber HID.SerialNumber -- ^ Serial does not have right format.
  | CouldNotReadSerial HID.DevicePath    -- ^ We could not read the serial from the device.
  | OtherEmotivException String
  deriving (Data, Typeable, Generic)

instance Exception EmotivException

instance Show EmotivException where
  show (InvalidSerialNumber sn)   = "Emotiv ERROR: the device serial number " ++ sn ++ " does not look valid"
  show (CouldNotReadSerial path)  = "Emotiv ERROR: could not read serial number of device " ++ path ++ ". Maybe you are not running as root?"
  show (OtherEmotivException err) = "Emotiv ERROR: " ++ err


-- | Identifies an Emotiv device.
data EmotivDeviceInfo = EmotivDeviceInfo
  { hidapiDeviceInfo :: DeviceInfo -- ^ The hidapi device info.
  } deriving (Show, Generic)


-- | An "open" data source to read bytes from.
data EmotivRawDevice
  = HidapiDevice
      { hidapiDevice :: HID.Device -- ^ The open hidapi device.
      }
  | HandleDevice
      { handleDevice :: Handle -- ^ A conventional `Handle`, e.g. an open file.
      } deriving (Generic)


-- | Identifies an open Emotiv device.
-- Also contains the cumulative `EmotivState` of the EEG.
data EmotivDevice = EmotivDevice
  { rawDevice    :: EmotivRawDevice           -- ^ Where we get our data from, some form of "open handle".
  , serial       :: SerialNumber              -- ^ The EEG's serial.
  , emotivModel  :: EmotivModel               -- ^ Whether the EEG is a consumer or developer model.
  , stateRef     :: IORef (Maybe EmotivState) -- ^ The EEG's cumulative state.
  } deriving (Generic)


-- | Conveniently expose the serial number of a device.
deviceInfoSerial :: EmotivDeviceInfo -> Maybe SerialNumber
deviceInfoSerial = (>>= makeSerialNumberFromString) . serialNumber . hidapiDeviceInfo


-- | Lists all EPOC devices, ordered by interface number.
-- If you do not actively choose amongst them, the last one is usually the one
-- you want (especially if only 1 EEG is connected).
getEmotivDevices :: IO [EmotivDeviceInfo]
getEmotivDevices = map EmotivDeviceInfo
                 . sortBy (comparing interfaceNumber)
                 <$> HID.enumerate (Just _EMOTIV_VENDOR_ID) (Just _EMOTIV_PRODUCT_ID)


-- | Opens a given Emotiv device.
-- Returns an `EmotivDevice` to read from with `readEmotiv`.
openEmotivDevice :: EmotivModel -> EmotivDeviceInfo -> IO EmotivDevice
openEmotivDevice model EmotivDeviceInfo{ hidapiDeviceInfo } = case hidapiDeviceInfo of
  DeviceInfo{ serialNumber = Nothing, path } -> throwIO $ CouldNotReadSerial path
  DeviceInfo{ serialNumber = Just sn } ->
    case makeSerialNumberFromString sn of
      Nothing -> throwIO $ InvalidSerialNumber sn
      Just s  -> do hidDev <- HID.openDeviceInfo hidapiDeviceInfo
                    stateRef <- newIORef Nothing
                    return $ EmotivDevice
                      { rawDevice   = HidapiDevice hidDev
                      , serial      = s
                      , stateRef    = stateRef
                      , emotivModel = model
                      }


-- | Creates an `EmotivDevice` device from a path, e.g. a device like
-- @/dev/hidraw1@ or a normal file containing dumped binary data.
openEmotivDeviceFile :: EmotivModel -> SerialNumber -> String -> IO EmotivDevice
openEmotivDeviceFile model sn path = do
  h <- openFile path ReadMode
  stateRef <- newIORef Nothing
  return $ EmotivDevice
    { rawDevice   = HandleDevice h
    , serial      = sn
    , stateRef    = stateRef
    , emotivModel = model
    }


-- | Reads one 32 byte packet from the device and decrypts it to raw data.
readEmotivRaw :: EmotivDevice -> IO EmotivRawData
readEmotivRaw EmotivDevice{ rawDevice, serial, emotivModel } = do

  d32 <- case rawDevice of HidapiDevice d -> HID.read d 32
                           HandleDevice d -> BS.hGet d 32

  return $ decrypt serial emotivModel d32


-- | Given a device and a Emotiv raw data, parses the raw data into an
-- `EmotivPacket` and updates the cumulative `EmotivState` that we maintain
-- for that device.
--
-- Care should be taken that raw data is fed into this function in the right
-- order (e.g. respecting the EEG's increasing sequence numbers and quality
-- updates).
--
-- This function is only neededif you want to obtain both raw data and
-- parsed packages.
-- If you are not interested in raw data, use `readEmotiv` instead.
--
-- Returns both the packet read from the device and the updated state.
updateEmotivState :: EmotivDevice -> EmotivRawData -> IO (EmotivState, EmotivPacket)
updateEmotivState EmotivDevice{ stateRef } rawData = do

  let p = parsePacket rawData

  -- Update accumulative state

  lastState <- readIORef stateRef

  let lastBattery   = maybe 0 battery lastState
      lastQualities = maybe (V.replicate (length allSensors) 0) qualities lastState

      newState = EmotivState
        { counter   = packetCounter p
        , battery   = maybe lastBattery id (packetBattery p)
        , gyroX     = packetGyroX p
        , gyroY     = packetGyroY p
        , sensors   = packetSensors p
        -- We can't get around an O(n) qualities vector copy here
        -- if we want `Eq EmotivState`. It's small enough anyway.
        , qualities = lastQualities `deepseq` case packetQuality p of
                        Nothing          -> lastQualities
                        Just (sensor, l) -> lastQualities V.// [(fromEnum sensor, l)]
        }

  writeIORef stateRef (Just newState)

  return (newState, p)


-- | Reads one 32 byte packet from the device, parses the raw bytes into an
-- `EmotivPacket` and updates the cumulative `EmotivState` that we maintain
-- for that device.
--
-- Returns both the packet read from the device and the updated state.
readEmotiv :: EmotivDevice -> IO (EmotivState, EmotivPacket)
readEmotiv device = updateEmotivState device =<< readEmotivRaw device


-- | Opens and reads from the last available device, giving all data from it
-- to the given function.
--
-- Intended for use with ghci.
--
-- Examples:
--
-- >withDataFromLastEEG Consumer print
-- >withDataFromLastEEG Consumer (print . packetQuality . snd)
-- >withDataFromLastEEG Consumer (putStrLn . unwords . map show . V.toList . qualities . fst)
withDataFromLastEEG :: EmotivModel -> ((EmotivState, EmotivPacket) -> IO a) -> IO a
withDataFromLastEEG model f = do
  devices <- getEmotivDevices
  device <- case devices of
    [] -> error "No devices found."
    _  -> openEmotivDevice model (last devices)
  forever $ readEmotiv device >>= f


-- * NFData instances
instance NFData EmotivDeviceInfo where rnf = genericRnf
instance NFData EmotivException where rnf = genericRnf
instance NFData EmotivPacket where rnf = genericRnf
instance NFData EmotivRawData where rnf = genericRnf
instance NFData EmotivState where rnf = genericRnf
instance NFData Sensor where rnf = genericRnf
