module GHC.Vis.GTK.Common (
  visSignal,
  visRunning,
  visState,
  visBoxes,
  evaluate,
  printOne,
  printAll
  )
  where

import Control.Concurrent
import Control.DeepSeq

import Data.List
import Data.IORef

import System.IO.Unsafe

import GHC.Vis.Internal
import GHC.Vis.Types
import GHC.HeapView

-- | Communication channel to the visualization
visSignal :: MVar Signal
visSignal = unsafePerformIO (newEmptyMVar :: IO (MVar Signal))

-- | Whether a visualization is currently running
visRunning :: MVar Bool
visRunning = unsafePerformIO (newMVar False)

visState :: IORef State
visState = unsafePerformIO $ newIORef $ State [] [] [] ([], [], (0,0,1,1)) [] [] (0,0) Nothing Nothing False

-- | All the visualized boxes
visBoxes :: MVar [(Box, String)]
visBoxes = unsafePerformIO (newMVar [] :: IO (MVar [(Box, String)]))

evaluate :: String -> IO ()
evaluate identifier = do (_,hm) <- printAll
                         show (map go hm) `deepseq` return ()
  where go (Box a,(Just n, y)) | n == identifier = seq a (Just n, y)
                                 | otherwise = (Just n, y)
        go (_,(x,y)) = (x,y)

printOne :: a -> IO String
printOne a = do
  bs <- readMVar visBoxes
  case findIndex (\(b,_) -> asBox a == b) bs of
    Just pos -> do
      t  <- parseBoxes bs
      return $ show (t !! pos)
    Nothing -> return "Add entry first"

printAll :: IO (String, HeapMap)
printAll = do
  bs <- readMVar visBoxes
  (t,(_,h,_)) <- parseBoxesHeap bs
  return (show t, h)
