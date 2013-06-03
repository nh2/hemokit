module Main where

import           Control.Concurrent (threadDelay, forkIO)
import           Control.Monad
import           Data.IORef
import           System.IO
import           Test.Robot
import           Graphics.XHB.Connection (connect)
import           Text.Show.Pretty (ppShow)

import           Hemokit


main :: IO ()
main = do

  devices <- getEmotivDevices

  putStrLn $ "AvailableDevices:\n" ++ ppShow devices

  device <- openEmotivDevice $ case devices of d:_ -> d
                                               []  -> error "no Epoc devices found"

  m'xConnection <- connect
  xy <- newIORef (0,0)

  case m'xConnection of
    Nothing -> return ()
    Just xc -> void . forkIO . forever $ do
      (x, y) <- readIORef xy
      print (x, y)
      writeIORef xy (0, 0)
      runRobotWithConnection (moveBy ((-x) `quot` 10) (y `quot` 10)) xc
      threadDelay 10000

  forever $ do
    emotivPacket <- readEmotivPacket device
    -- print (qualities emotivPacket)
    print emotivPacket
    -- putStrLn $ show (gyroX emotivPacket) ++ " " ++ show (gyroY emotivPacket)
    hFlush stdout

    modifyIORef' xy $ \(x,y) -> (x + gyroX emotivPacket, y + gyroY emotivPacket)

    return (battery emotivPacket, quality emotivPacket)
