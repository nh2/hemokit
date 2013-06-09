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
    (state, _) <- readEmotiv device
    -- print (qualities state)
    print state
    -- putStrLn $ show (gyroX state) ++ " " ++ show (gyroY state)
    hFlush stdout

    modifyIORef' xy $ \(x,y) -> (x + gyroX state, y + gyroY state)

    return (battery state, qualities state)
