{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Concurrent
import Control.Monad
import Data.Vector ((!))
-- import Data.IORef
-- import qualified Data.Vector as V
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
-- import Graphics.UI.Gtk.Gdk.EventM
import Graphics.Rendering.Cairo.SVG

import Hemokit


main :: IO ()
main = do
  svg <- svgNewFromFile "images/head-map.svg"
  let (width, height) = svgGetSize svg
      drawSensor (xi, yi) _sensor qual = do
        let (x, y) = (fromIntegral xi, fromIntegral yi)
            -- name   = show sensor ++ " " ++ show qual
            name   = show qual
        setRed   >> drawCircle x y 15
        TextExtents{ textExtentsWidth = w } <- textExtents name
        moveTo (x - 8 - w/2) (y - 8)
        setBlack >> drawStr name

  emotivStateMvar <- newEmptyMVar
  forkIO $ do
    putMVar emotivStateMvar undefined
    putStrLn "Waiting for EEG data..."
    withDataFromLastEEG Consumer (void . swapMVar emotivStateMvar . fst)

  run width height $ \window -> do
    _ <- svgRender svg
    liftIO $ flush

    EmotivState{ counter, qualities } <- liftIO $ readMVar emotivStateMvar
    liftIO $ print (counter, qualities)

    forM_ allSensors $ \s ->
      drawSensor (sensorPosition (width, height) s) s (qualities ! (fromEnum s))

    liftIO $ flush >> widgetQueueDraw window


sensorPosition :: (Int, Int) -> Sensor -> (Int, Int)
sensorPosition (width, height) sensor = case sensor of

  -- Left side
  AF3 -> ( 125 , height - 434 )
  F3  -> ( 145 , height - 380 )
  F7  -> (  77 , height - 360 )
  FC5 -> ( 107 , height - 318 )
  T7  -> (  55 , height - 250 )
  -- CMS -> (  51 , height - 178 )
  -- F7  -> (  51 , height - 178 )
  -- CMS -> ( 102 , height - 153 )
  P7  -> (  80 , height -  93 )
  O1  -> ( 147 , height -  35 )

  -- Right side
  AF4 -> yMirror AF3
  F4  -> yMirror F3
  F8  -> yMirror F7
  FC6 -> yMirror FC5
  T8  -> yMirror T7
  -- DRL -> yMirror CMS
  -- F8  -> yMirror F7
  -- DRL -> yMirror CMS
  P8  -> yMirror P7
  O2  -> yMirror O1
  where
    yMirror s = let (x,y) = sensorPosition (width, height) s
                 in (width - x, y)



drawCircle :: Double -> Double -> Double -> Render ()
drawCircle x y radius = do
  arc x y radius 0 (2 * pi)
  -- stroke
  fill
  -- fillPreserve



run :: Int -> Int -> (Window -> Render ()) -> IO ()
run width height renderAct = do
  _ <- initGUI
  win <- windowNew
  vbox <- vBoxNew False 0
  closeButton <- buttonNewWithLabel "Close"
  onClicked closeButton (widgetDestroy win)
  canvas <- drawingAreaNew
  _ <- canvas `onSizeRequest` return (Requisition width height)
  _ <- canvas `on` exposeEvent $ tryEvent $ updateCanvas canvas (renderAct win)

  containerAdd vbox closeButton
  containerAdd vbox canvas
  containerAdd win vbox

  widgetShow canvas
  widgetShowAll win
  onDestroy win mainQuit
  -- flush
  mainGUI
  -- Flush all commands that are waiting to be sent to the graphics server.
  -- This ensures that the window is actually closed before ghci displays the
  -- prompt again.

  where
    updateCanvas :: DrawingArea -> Render () -> EventM EExpose ()
    updateCanvas canvas act = liftIO $ do
      win <- widgetGetDrawWindow canvas
      renderWithDrawable win act


setRed :: Render ()
setRed = do
  setSourceRGB 1 0 0

setBlack :: Render ()
setBlack = do
  setSourceRGB 0 0 0


drawStr :: String -> Render ()
drawStr txt = do
  lay <- createLayout txt
  showLayout lay
