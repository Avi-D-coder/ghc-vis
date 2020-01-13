{-# LANGUAGE CPP, ImpredicativeTypes, OverloadedStrings #-}
{- |
   Module      : GHC.Vis
   Copyright   : (c) Dennis Felsing
   License     : 3-Clause BSD-style
   Maintainer  : dennis@felsin9.de

Although ghc-vis is meant to be used in GHCi it can also be used as a library
in regular Haskell programs which are run or compiled by GHC. You can run those
programs using \"runghc example.hs\" or \"ghc -threaded example.hs && ./example\".
Without the \"-threaded\"-Flag ghc-vis does not work correctly. This is an
example using ghc-vis outside of GHCi:

> import GHC.Vis
>
> main = do
>   putStrLn "Start"
>   let a = "teeest"
>   let b = [1..3]
>   let c = b ++ b
>   let d = [1..]
>   putStrLn $ show $ d !! 1
>
>   visualization
>   view a "a"
>   view b "b"
>   view c "c"
>   view d "d"
>
>   getChar
>   switch
>
>   getChar
>   putStrLn "End"
 -}
module GHC.Vis (
  vis,
  mvis,
#ifdef SDL_WINDOW
  svis,
#endif
  view,
  eval,
  switch,
  update,
  clear,
  restore,
  history,
  setDepth,
  export
  )
  where

#if __GLASGOW_HASKELL__ < 706
import Prelude hiding (catch)
#else
import Prelude
#endif

import Graphics.UI.Gtk hiding (Box, Signal)

import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Control.Exception hiding (evaluate)

import Data.Char
import Data.Version

import Data.Text ()
import Data.GraphViz.Commands

import GHC.HeapView hiding (name)

import GHC.Vis.Types hiding (view)
import qualified GHC.Vis.Types as T
import GHC.Vis.View.Common
import qualified GHC.Vis.View.List as List

#ifdef GRAPH_VIEW
import qualified GHC.Vis.View.Graph as Graph
#endif

import Graphics.Rendering.Cairo hiding (restore, x, y, width, height)

#ifdef FULL_WINDOW
import Graphics.Rendering.Cairo.SVG
import Paths_ghc_vis as My
#endif

#ifdef SDL_WINDOW
import qualified Graphics.UI.SDL as SDL
import Foreign.Ptr ( castPtr )
#endif

views :: [View]
views =
  View List.redraw List.click List.move List.updateObjects List.export :
#ifdef GRAPH_VIEW
  View Graph.redraw Graph.click Graph.move Graph.updateObjects Graph.export :
#endif
  []

title :: String
title = "ghc-vis"

backgroundColor :: Color
backgroundColor = Color 0xffff 0xffff 0xffff

defaultSize :: (Int, Int)
defaultSize = (640, 480)

zoomIncrement :: Double
zoomIncrement = 1.25

positionIncrement :: Double
positionIncrement = 50

bigPositionIncrement :: Double
bigPositionIncrement = 200

#ifdef SDL_WINDOW
black = SDL.Pixel maxBound
white = SDL.Pixel 0xFFFFFF
#endif

-- | This is the main function. It's to be called from GHCi and launches a
--   graphical window in a new thread.
vis :: IO ()
#ifdef FULL_WINDOW
vis = visStart visMainThread
#else
vis = mvis
#endif

visStart :: IO () -> IO ()
visStart main = do
  vr <- atomically $ do
    r <- readTVar visRunning
    unless r $ writeTVar visRunning True
    return r
  unless vr $ void $ forkIO main

-- | A minimalistic version of ghc-vis, without window decorations, help and
--   all that other stuff.
mvis :: IO ()
mvis = visStart mVisMainThread

#ifdef SDL_WINDOW
-- | SDL version. Not properly working yet. Mainly for testing whether SDL
--   works better than GTK on some platforms.
svis :: IO ()
svis = do
  vr <- swapMVar visRunning True
  unless vr $ void $ forkIO sdlVisMainThread
#endif

-- | Add expressions with a name to the visualization window.
view :: a -> String -> IO ()
view a name = atomically $ put $ NewSignal (asBox a) name

-- | Evaluate an object that is shown in the visualization. (Names start with 't')
eval :: String -> IO ()
eval t = evaluate t >> update

-- | Switch between the list view and the graph view
switch :: IO ()
switch = aPut SwitchSignal

-- | When an object is updated by accessing it, you have to call this to
--   refresh the visualization window. You can also click on an object to force
--   an update.
update :: IO ()
update = aPut UpdateSignal

-- | Clear the visualization window, removing all expressions from it
clear :: IO ()
clear = aPut ClearSignal

-- | Reset the hidden boxes
restore :: IO ()
restore = aPut RestoreSignal

-- | Change position in history
history :: (Int -> Int) -> IO ()
history = aPut . HistorySignal

-- | Set the maximum depth for following closures on the heap
setDepth :: Int -> IO ()
setDepth newDepth
  | newDepth > 0 = atomically $ modifyTVar' visState (\s -> s {heapDepth = newDepth})
  | otherwise    = error "Heap depth has to be positive"

zoom :: WidgetClass w => w -> (Double -> Double) -> STM (IO ())
zoom canvas f = do
  state <- readTVar visState

  let newZoomRatio = f $ zoomRatio state
      newPos = zoomImage canvas state newZoomRatio (mousePos state)
  modifyTVar' visState (\s -> s {zoomRatio = newZoomRatio, position = newPos})

  return $ widgetQueueDraw canvas

movePos :: WidgetClass w => w -> (T.Point -> T.Point) -> IO ()
movePos canvas f = do
  atomically $ modifyTVar' visState (\s ->
    let newPosition = f $ position s
    in s {position = newPosition})
  widgetQueueDraw canvas

-- | Export the current visualization view to a file, format depends on the
--   file ending. Currently supported: svg, png, pdf, ps
export :: String -> IO ()
export filename = void $ export' filename

export' :: String -> IO (Maybe String)
export' filename = case mbDrawFn of
  Right errorMsg -> do putStrLn errorMsg
                       return $ Just errorMsg
  Left _ -> atomically $ do
              put $ ExportSignal ((\(Left x) -> x) mbDrawFn) filename
              return (Nothing :: Maybe String)

  where mbDrawFn = case map toLower (reverse . take 4 . reverse $ filename) of
          ".svg"  -> Left withSVGSurface
          ".pdf"  -> Left withPDFSurface
          ".png"  -> Left withPNGSurface
          _:".ps" -> Left withPSSurface
          _       -> Right "Unknown file extension, try one of the following: .svg, .pdf, .ps, .png"

        withPNGSurface :: FilePath -> Double -> Double -> (Surface -> IO a) -> IO a
        withPNGSurface filePath width height action =
          withImageSurface FormatARGB32 (ceiling width) (ceiling height) $
          \surface -> do
            ret <- action surface
            surfaceWriteToPNG surface filePath
            return ret

put :: Signal -> STM ()
put = writeTQueue visSignal

aPut :: Signal -> IO ()
aPut sig = atomically $ writeTQueue visSignal sig

mVisMainThread :: IO ()
mVisMainThread = do
  initGUI
  window <- windowNew

  canvas <- drawingAreaNew

  widgetModifyBg canvas StateNormal backgroundColor

  set window [ windowTitle := title
             , containerChild := canvas
             ]
  (uncurry $ windowSetDefaultSize window) defaultSize

  s <- readTVarIO visState

  on canvas draw $ do
    runCorrect s redraw canvas
    liftIO $ join $ atomically $ runCorrect s move canvas

  dummy <- windowNew

  setupGUI window canvas dummy

#ifdef SDL_WINDOW
sdlVisMainThread :: IO ()
sdlVisMainThread = SDL.withInit [ SDL.InitVideo ] $ do
  screen <- SDL.setVideoMode 600 600 32 [ SDL.Resizable ]

  SDL.fillRect screen Nothing black

  pixels <- fmap castPtr $ SDL.surfaceGetPixels screen

  -- Here we test the first of the user facing functions added by the patch.
  -- Be sure to use FORMATRGB24 so Cairo and SDL don't conflict about alpha
  -- values. If you need to use alpha channel in your Cairo routines, consider
  -- using an independent Cairo surface to buffer your alpha drawing to.
  --renderWith canvas demo1

  -- And here is the other new function for when you want to hide the
  -- Cairo.Surface from the caller.

  canvas <- createImageSurfaceForData pixels FormatRGB24 600 600 (600 * 4)

  reactThread <- forkIO $ react2

  let idle = do
        e <- SDL.waitEvent
        case e of
          SDL.Quit -> quit reactThread
          otherwise -> do
            putStrLn "Updating"
            s <- List.getState
            SDL.fillRect screen Nothing white
            boundingBoxes <- renderWith canvas $ List.draw s 600 600
            List.updateBoundingBoxes boundingBoxes
            SDL.flip screen
            idle

  idle
#endif

setupGUI :: (WidgetClass w1, WidgetClass w2, WidgetClass w3) => w1 -> w2 -> w3 -> IO ()
setupGUI window canvas legendCanvas = do
  widgetAddEvents canvas [PointerMotionMask]
  on canvas motionNotifyEvent $ do
    (x,y) <- eventCoordinates
    lift $ atomically $ do
        state <- readTVar visState
        modifyTVar' visState (\s -> s {mousePos = (x,y)})

        if dragging state
        then do
          let (oldX, oldY) = mousePos state
              (deltaX, deltaY) = (x - oldX, y - oldY)
              (oldPosX, oldPosY) = position state
          modifyTVar' visState (\s -> s {position = (oldPosX + deltaX, oldPosY + deltaY)})
          return $ widgetQueueDraw canvas
        else do
          s <- readTVar visState
          runCorrect s move canvas
    return True

  on canvas buttonPressEvent $ do
    button <- eventButton
    eClick <- eventClick
    state <- liftIO $ readTVarIO visState
    lift $ do
      when (button == LeftButton && eClick == SingleClick) $
        runCorrect state click

      when (button == RightButton && eClick == SingleClick) $
        atomically $ modifyTVar' visState (\s -> s {dragging = True})

      when (button == MiddleButton && eClick == SingleClick) $ do
        atomically $ modifyTVar' visState (\s -> s {zoomRatio = 1, position = (0, 0)})
        widgetQueueDraw canvas

    return True

  on canvas buttonReleaseEvent $ do
    button <- eventButton
    lift $ do
      when (button == RightButton) $
        atomically $ modifyTVar' visState (\s -> s {dragging = False})

      return True

  on canvas scrollEvent $ do
    direction <- eventScrollDirection
    lift $ do
      atomically $ do
        state <- readTVar visState

        when (direction == ScrollUp) $ do
          let newZoomRatio = zoomRatio state * zoomIncrement
              newPos = zoomImage canvas state newZoomRatio (mousePos state)
          modifyTVar' visState (\s -> s {zoomRatio = newZoomRatio, position = newPos})

        when (direction == ScrollDown) $ do
          let newZoomRatio = zoomRatio state / zoomIncrement
              newPos = zoomImage canvas state newZoomRatio (mousePos state)
          modifyTVar' visState (\s -> s {zoomRatio = newZoomRatio, position = newPos})

      widgetQueueDraw canvas
      return True

  on window keyPressEvent $ do
    eKeyName <- eventKeyName

    when (eKeyName `elem` ["space", "Return", "KP_Enter"]) $ lift $ do
      state <- readTVarIO visState
      runCorrect state click

    lift $ do
      atomically $ do
        state <- readTVar visState

        when (eKeyName `elem` ["plus", "Page_Up", "KP_Add"]) $ do
          let newZoomRatio = zoomRatio state * zoomIncrement
              (oldX, oldY) = position state
              newPos = (oldX*zoomIncrement, oldY*zoomIncrement)
          modifyTVar' visState (\s -> s {zoomRatio = newZoomRatio, position = newPos})

        when (eKeyName `elem` ["minus", "Page_Down", "KP_Subtract"]) $ do
          let newZoomRatio = zoomRatio state / zoomIncrement
              (oldX, oldY) = position state
              newPos = (oldX/zoomIncrement, oldY/zoomIncrement)
          modifyTVar' visState (\s -> s {zoomRatio = newZoomRatio, position = newPos})

        when (eKeyName `elem` ["0", "equal"]) $
          modifyTVar' visState (\s -> s {zoomRatio = 1, position = (0, 0)})

        when (eKeyName `elem` ["Left", "h", "a"]) $
          modifyTVar' visState (\s ->
            let (x,y) = position s
                newX  = x + positionIncrement
            in s {position = (newX, y)})

        when (eKeyName `elem` ["Right", "l", "d"]) $
          modifyTVar' visState (\s ->
            let (x,y) = position s
                newX  = x - positionIncrement
            in s {position = (newX, y)})

        when (eKeyName `elem` ["Up", "k", "w"]) $
          modifyTVar' visState (\s ->
            let (x,y) = position s
                newY  = y + positionIncrement
            in s {position = (x, newY)})

        when (eKeyName `elem` ["Down", "j", "s"]) $
          modifyTVar' visState (\s ->
            let (x,y) = position s
                newY  = y - positionIncrement
            in s {position = (x, newY)})

        when (eKeyName `elem` ["H", "A"]) $
          modifyTVar' visState (\s ->
            let (x,y) = position s
                newX  = x + bigPositionIncrement
            in s {position = (newX, y)})

        when (eKeyName `elem` ["L", "D"]) $
          modifyTVar' visState (\s ->
            let (x,y) = position s
                newX  = x - bigPositionIncrement
            in s {position = (newX, y)})

        when (eKeyName `elem` ["K", "W"]) $
          modifyTVar' visState (\s ->
            let (x,y) = position s
                newY  = y + bigPositionIncrement
            in s {position = (x, newY)})

        when (eKeyName `elem` ["J", "S"]) $
          modifyTVar' visState (\s ->
            let (x,y) = position s
                newY  = y - bigPositionIncrement
            in s {position = (x, newY)})

        when (eKeyName == "v") $
          put SwitchSignal

        when (eKeyName == "c") $
          put ClearSignal

        when (eKeyName == "C") $
          put RestoreSignal

        when (eKeyName == "u") $
          put UpdateSignal

        when (eKeyName `elem` ["comma", "bracketleft"]) $
          put $ HistorySignal (+1)

        when (eKeyName `elem` ["period", "bracketright"]) $
          put $ HistorySignal (\x -> x - 1)

      widgetQueueDraw canvas
      return True

  widgetShowAll window

  reactThread <- forkIO $ react canvas legendCanvas
  --onDestroy window mainQuit -- Causes :r problems with multiple windows
  on window destroyEvent $ lift $ quit reactThread >> return True

  mainGUI
  return ()

quit :: ThreadId -> IO ()
quit reactThread = do
  atomically $ writeTVar visRunning False
  killThread reactThread

#ifdef SDL_WINDOW
react2 :: IO b
react2 = do
  -- Timeout used to handle ghci reloads (:r)
  -- Reloads cause the visSignal to be reinitialized, but takeMVar is still
  -- waiting for the old one.  This solution is not perfect, but it works for
  -- now.
  mbSignal <- timeout signalTimeout (takeMVar visSignal)
  case mbSignal of
    Nothing -> do
      running <- readTVar visRunning
      if running then react2 else
        -- :r caused visRunning to be reset
        (do swapMVar visRunning True
            timeout signalTimeout (putMVar visSignal UpdateSignal)
            react2)
    Just signal -> do
      case signal of
        NewSignal x n  -> modifyMVar_ visBoxes (
          \y -> return $ if (x,n) `elem` y then y else y ++ [(x,n)])
        ClearSignal    -> modifyMVar_ visBoxes (\_ -> return [])
        UpdateSignal   -> return ()
        SwitchSignal   -> doSwitch
        ExportSignal d f -> catch (runCorrect exportView >>= \e -> e d f)
          (\e -> do let err = show (e :: IOException)
                    hPutStrLn stderr $ "Couldn't export to file \"" ++ f ++ "\": " ++ err
                    return ())

      boxes <- readTVar visBoxes
      performGC -- Else Blackholes appear. Do we want this?
                -- Blackholes stop our current thread and only resume after
                -- they have been replaced with their result, thereby leading
                -- to an additional element in the HeapMap we don't want.
                -- Example for bad behaviour that would happen then:
                -- λ> let xs = [1..42] :: [Int]
                -- λ> let x = 17 :: Int
                -- λ> let ys = [ y | y <- xs, y >= x ]

      runCorrect updateObjects >>= \f -> f boxes

      --postGUISync $ widgetQueueDraw canvas
      --postGUISync $ widgetQueueDraw legendCanvas
      react2

#ifdef GRAPH_VIEW
  where doSwitch = isGraphvizInstalled >>= \gvi -> if gvi
          then atomically $ modifyTVar' visState (\s -> s {T.view = succN (T.view s), zoomRatio = 1, position = (0, 0)})
          else putStrLn "Cannot switch view: The Graphviz binary (dot) is not installed"

        succN GraphView = ListView
        succN ListView = GraphView
#else
  where doSwitch = putStrLn "Cannot switch view: Graph view disabled at build"
#endif
#endif

react :: (WidgetClass self1, WidgetClass self2) =>
           self1 -> self2 -> IO a
react canvas legendCanvas = do
  mbSignal <- atomically $ tryPeekTQueue visSignal
  case mbSignal of
    Nothing -> liftIO $ do
      threadDelay 12000
      react canvas legendCanvas
    Just _ -> liftIO $ do
      (signal, s, boxes) <- atomically reactStm
      case signal of
        ExportSignal d f ->
          catch
            (runCorrect s exportView d f)
            ( \e -> do
                let err = show (e :: IOException)
                hPutStrLn stderr $ "Couldn't export to file \"" ++ f ++ "\": " ++ err
                return ()
            )
        SwitchSignal -> do
          let succN GraphView = ListView
              succN ListView = GraphView
#ifdef GRAPH_VIEW
          isGraphvizInstalled >>= \gvi -> if gvi
            then atomically $ modifyTVar' visState (\s' -> s' {T.view = succN (T.view s'), zoomRatio = 1, position = (0, 0)})
            else putStrLn "Cannot switch view: The Graphviz binary (dot) is not installed"
#else
          putStrLn "Cannot switch view: Graph view disabled at build"
#endif
        _ -> return ()
      runCorrect s updateObjects boxes
      postGUISync $ widgetQueueDraw canvas
      -- postGUISync $ widgetQueueDraw legendCanvas
      react canvas legendCanvas

reactStm :: STM (Signal, State, [NamedBox])
reactStm = do
  mbSignal <- readTQueue visSignal
  case mbSignal of
    signal -> do
      case signal of
        NewSignal x n ->
          modifyTVar' visBoxes (([n], x):)
        ClearSignal -> do
          modifyTVar' visBoxes $ const []
          modifyTVar' visHidden $ const []
          writeTVar visHeapHistory (-1)
        RestoreSignal ->
          modifyTVar' visHidden $ const []
        HistorySignal f -> do
          xs <- readTVar visBoxes
          modifyTVar' visHeapHistory $ \i -> max 0 (min (length xs - 1) (f i))
        _ -> return ()
      s <- readTVar visState
      boxes <- readTVar visBoxes
      return (signal, s, boxes)

runCorrect :: State -> (View -> a) -> a
runCorrect s f = f $ views !! fromEnum (T.view s)

zoomImage :: WidgetClass w1 => w1 -> State -> Double -> T.Point -> T.Point
zoomImage _canvas s newZoomRatio _mousePos@(_x', _y') =
  let (oldPosX, oldPosY) = position s
      newZoom = newZoomRatio / zoomRatio s
      newPos = (oldPosX * newZoom, oldPosY * newZoom)
  in newPos

#ifdef FULL_WINDOW
visMainThread :: IO ()
visMainThread = do
  initGUI

  builder <- builderNew
  builderAddFromFile builder =<< My.getDataFileName "data/main.ui"

  let getO :: forall cls . GObjectClass cls
           => (GObject -> cls)
           -> String
           -> IO cls
      getO = builderGetObject builder

  window       <- getO castToWindow "window"
  canvas       <- getO castToDrawingArea "drawingarea"

  saveDialog   <- getO castToFileChooserDialog "savedialog"
  aboutDialog  <- getO castToAboutDialog "aboutdialog"

  depthDialog  <- getO castToDialog "depthdialog"
  depthSpin    <- getO castToSpinButton "depthspin"

  legendDialog <- getO castToWindow "legenddialog"
  legendCanvas <- getO castToDrawingArea "legenddrawingarea"

  newFilter "*.pdf" "PDF" saveDialog
  newFilter "*.svg" "SVG" saveDialog
  newFilter "*.ps" "PostScript" saveDialog
  newFilter "*.png" "PNG" saveDialog

  set aboutDialog [aboutDialogVersion := showVersion My.version]

  on saveDialog  response $ fileSave saveDialog
  on depthDialog response $ setDepthDialog depthDialog depthSpin
  on aboutDialog response $ const $ widgetHide aboutDialog

  on saveDialog   deleteEvent $ lift $ widgetHide saveDialog   >> return True
  on aboutDialog  deleteEvent $ lift $ widgetHide aboutDialog  >> return True
  on legendDialog deleteEvent $ lift $ widgetHide legendDialog >> return True

  let setDepthSpin = do
        s <- readTVarIO visState
        spinButtonSetValue depthSpin $ fromIntegral $ heapDepth s


  getO castToMenuItem "clear"       >>= \item -> on item menuItemActivated clear
  getO castToMenuItem "switch"      >>= \item -> on item menuItemActivated switch
  getO castToMenuItem "restore"     >>= \item -> on item menuItemActivated restore
  getO castToMenuItem "update"      >>= \item -> on item menuItemActivated update
  getO castToMenuItem "setdepth"    >>= \item -> on item menuItemActivated $ setDepthSpin >> widgetShow depthDialog
  getO castToMenuItem "export"      >>= \item -> on item menuItemActivated $ widgetShow saveDialog
  getO castToMenuItem "quit"        >>= \item -> on item menuItemActivated $ widgetDestroy window
  getO castToMenuItem "about"       >>= \item -> on item menuItemActivated $ widgetShow aboutDialog
  getO castToMenuItem "legend"      >>= \item -> on item menuItemActivated $ widgetShow legendDialog
  getO castToMenuItem "timeback"    >>= \item -> on item menuItemActivated $ history (+1)
  getO castToMenuItem "timeforward" >>= \item -> on item menuItemActivated $ history (\x -> x - 1)
  getO castToMenuItem "zoomin"      >>= \item -> on item menuItemActivated $ join $ atomically $ zoom canvas (*1.25)
  getO castToMenuItem "zoomout"     >>= \item -> on item menuItemActivated $ join $ atomically $ zoom canvas (/1.25)
  getO castToMenuItem "left"        >>= \item -> on item menuItemActivated $ movePos canvas (\(x,y) -> (x + positionIncrement, y))
  getO castToMenuItem "right"       >>= \item -> on item menuItemActivated $ movePos canvas (\(x,y) -> (x - positionIncrement, y))
  getO castToMenuItem "up"          >>= \item -> on item menuItemActivated $ movePos canvas (\(x,y) -> (x, y + positionIncrement))
  getO castToMenuItem "down"        >>= \item -> on item menuItemActivated $ movePos canvas (\(x,y) -> (x, y - positionIncrement))

  widgetModifyBg canvas StateNormal backgroundColor
  widgetModifyBg legendCanvas StateNormal backgroundColor

  welcomeSVG <- My.getDataFileName "data/welcome.svg" >>= svgNewFromFile

  legendListSVG  <- My.getDataFileName "data/legend_list.svg" >>= svgNewFromFile
  legendGraphSVG <- My.getDataFileName "data/legend_graph.svg" >>= svgNewFromFile

  on canvas draw $ do
    boxes <- liftIO $ atomically $ readTVar visBoxes
    s <- liftIO $ readTVarIO visState

    if null boxes
    then renderSVGScaled canvas welcomeSVG
    else do
      runCorrect s redraw canvas
      liftIO $ atomically $ runCorrect s move canvas
      return ()

  on legendCanvas draw $ do
    state <- liftIO $ readTVarIO visState
    renderSVGScaled legendCanvas $ case T.view state of
      ListView  -> legendListSVG
      GraphView -> legendGraphSVG

  setupGUI window canvas legendCanvas

fileSave :: FileChooserDialog -> ResponseId -> IO ()
fileSave fcdialog responseId = do
  case responseId of
    ResponseOk -> do Just filename <- fileChooserGetFilename fcdialog
                     mbError <- export' filename
                     case mbError of
                       Nothing -> return ()
                       Just errorMsg -> do
                         errorDialog <- messageDialogNew Nothing [] MessageError ButtonsOk errorMsg
                         widgetShow errorDialog
                         on errorDialog response $ const $ widgetHide errorDialog
                         return ()
    _ -> return ()
  widgetHide fcdialog

setDepthDialog :: Dialog -> SpinButton -> ResponseId -> IO ()
setDepthDialog depthDialog depthSpin responseId = do
  case responseId of
    ResponseOk -> do depth <- spinButtonGetValue depthSpin
                     setDepth $ round depth
    _ -> return ()
  widgetHide depthDialog

newFilter :: FileChooserClass fc => String -> String -> fc -> IO ()
newFilter filterString name dialog = do
  filt <- fileFilterNew
  fileFilterAddPattern filt filterString
  fileFilterSetName filt $ name ++ " (" ++ filterString ++ ")"
  fileChooserAddFilter dialog filt

renderSVGScaled :: (WidgetClass w) => w -> SVG -> Render ()
renderSVGScaled canvas svg = do
  rw2 <- liftIO $ widgetGetAllocatedWidth canvas
  rh2 <- liftIO $ widgetGetAllocatedHeight canvas
  let (cx2, cy2) = svgGetSize svg

      (rw,rh) = (fromIntegral rw2, fromIntegral rh2)
      (cx,cy) = (fromIntegral cx2, fromIntegral cy2)

      -- Proportional scaling
      (sx,sy) = (min (rw/cx) (rh/cy), sx)
      (ox,oy) = (rw/2 - sx*cx/2, rh/2 - sy*cy/2)

  translate ox oy
  scale sx sy
  svgRender svg
  return ()
#endif

-- Zoom into mouse, but only working from (0,0)
-- newPos = ( oldPosX + x * zoomRatio s - x * newZoomRatio
--          , oldPosY + y * zoomRatio s - y * newZoomRatio )

-- Zoom into center:
-- newPos = ( oldPosX * zoomIncrement
--          , oldPosY * zoomIncrement )
