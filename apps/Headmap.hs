module Main where

import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
-- import Graphics.UI.Gtk.Gdk.EventM
import Graphics.Rendering.Cairo.SVG

main :: IO ()
main = do
  svg <- svgNewFromFile "images/head-map.svg"
  let (width, height) = svgGetSize svg
      sensorL x bottomY name = do
        let y = fromIntegral height - bottomY
        setRed   >> drawCircle x y 15
        TextExtents{ textExtentsWidth = w } <- textExtents name
        moveTo (x - 8 - w/2) (y - 8)
        setBlack >> drawStr name
      sensorR rightX bottomY name = do
        let y = fromIntegral height - bottomY
            x = fromIntegral width - rightX
        setRed   >> drawCircle x y 15
        TextExtents{ textExtentsWidth = w } <- textExtents name
        moveTo (x - 8 - w/2) (y - 8)
        setBlack >> drawStr name

  run width height $ do
    _ <- svgRender svg

    -- Left side
    sensorL 125 434 "AF3" -- AF3
    sensorL 145 380 "F3"  -- F3
    sensorL  77 360 "F7"  -- F7
    sensorL 107 318 "FC5" -- FC5
    sensorL  55 250 "T7"  -- T7
    sensorL  51 178 "CMS" -- CMS low
    sensorL 102 153 "CMS" -- CMS high
    sensorL  80  93 "P7"  -- P7
    sensorL 147  35 "O1"  -- O1

    sensorR 125 434 "AF4" -- AF4
    sensorR 145 380 "F4"  -- F4
    sensorR  77 360 "F7"  -- F7
    sensorR 107 318 "FC6" -- FC6
    sensorR  55 250 "T8"  -- T8
    sensorR  51 178 "DRL" -- DRL low
    sensorR 102 153 "DRL" -- DRL high
    sensorR  80  93 "P8"  -- P8
    sensorR 147  35 "O2"  -- O2

    return ()


drawCircle :: Double -> Double -> Double -> Render ()
drawCircle x y radius = do
  arc x y radius 0 (2 * pi)
  -- stroke
  fill
  -- fillPreserve



run :: Int -> Int -> Render () -> IO ()
run width height renderAct = do
  _ <- initGUI
  dia <- dialogNew
  _ <- dialogAddButton dia stockClose ResponseClose
  contain <- dialogGetUpper dia
  canvas <- drawingAreaNew
  _ <- canvas `onSizeRequest` return (Requisition width height)
  _ <- canvas `on` exposeEvent $ tryEvent $ updateCanvas canvas renderAct
  boxPackStartDefaults contain canvas
  widgetShow canvas
  _ <- dialogRun dia
  widgetDestroy dia
  -- Flush all commands that are waiting to be sent to the graphics server.
  -- This ensures that the window is actually closed before ghci displays the
  -- prompt again.
  flush

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
