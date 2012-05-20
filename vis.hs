{-# LANGUAGE MagicHash, BangPatterns #-}

module GHC.Vis (
  walkHeap,
  dprint,
  bprint,
  mprint,
  eval,
  eval2,
  eval3
  )
  where

import GHC.HeapView
import System.Mem
import System.Mem.StableName
import System.IO.Unsafe

import Control.DeepSeq
import Control.Monad
import Control.Monad.State
import Data.Tree
import Data.Word
import Data.Char
import Data.List
import Data.Maybe
import GHC.Prim
import GHC.Exts
import Text.Printf
import Unsafe.Coerce

import Data.Map (Map)
import qualified Data.Map as Map

fromJust2 :: String -> Maybe a -> a
fromJust2 _ (Just a) = a
fromJust2 s Nothing = error ("fromJust" ++ s)

--import Data.IntMap(IntMap)
--import qualified Data.IntMap as IntMap

type HeapEntry = (Maybe String, Closure)
type HeapMap   = Map Box HeapEntry
--type PrintState = State HeapMap
type PrintState = State (Integer, HeapMap)

instance Ord Box
  -- TODO: Doesn't work!
  -- Temporarily, probably use System.Mem.StableName
  --where compare x y = compare (show x) (show y)
  -- Ok, this works for now
  where compare (Box x) (Box y) = unsafePerformIO $ (do
          a <- makeStableName x
          b <- makeStableName y
          return $ compare (hashStableName a) (hashStableName b)
          )

buildTree x = do
  cd <- getBoxedClosureData x
  return (cd, allPtrs cd)

t x = unfoldTreeM buildTree x

u = t . asBox

-- infinite tree duh -- needs MVAR_CLEAN at some weird depth (depends on run)
f = let y = id (:) () y
    in getClosureData y

g = let y = id (:) () y
    in y

h x = id (:) x (h x)

isM (MVarClosure _ _ _ _) = True
isM _ = False

isF (FunClosure _ _ _) = True
isF _ = False

--boxAddr :: Box -> Int
--boxAddr (Box any) = fromIntegral $ W# (aToWord# any)

-- Don't inspect deep pointers in BCOClosures for now, they never end
-- TODO: What we could do is output the instrs and literals of it
nearlyAllPtrs (BCOClosure (StgInfoTable _ _ _ _) i l _b _ _ _) = [i,l]
nearlyAllPtrs x = allPtrs x

-- Run performGC from System.Mem first to get pointers not to change as much
-- while executing
walkHeap :: a -> IO HeapMap
walkHeap a = do
  performGC
  go (asBox a) Map.empty
  where go b l = case Map.lookup b l of
                   Just _  -> return l
                   Nothing -> do
                     c' <- getBoxedClosureData b
                     l' <- foldM (\l x -> go x l) (Map.insert b (Nothing, c') l) (nearlyAllPtrs c')
                     return $ (Map.insert b (Nothing, c') l')

--walkHeapDepth a d = go (asBox a) d IntMap.empty
--  where go b d im | d == 0 = return im
--                  | (boxAddr b) `IntMap.member` im = do
--                      return im
--                  | otherwise = do
--                      c <- getBoxedClosureData b
--                      --ims <- liftM IntMap.unions (sequence (map (\x -> go x im) (allPtrs c)))
--                      ims <- foldM (\im x -> go x (d - 1) im) im (allPtrs c)
--                      return $ (ins b c ims)

-- TODO: Proper forced evalution
eval :: String -> HeapMap -> HeapMap
eval name hm = Map.mapWithKey go hm
  where go (Box a) (Just n, y) | n == name = seq a (Just n, y)
                               | otherwise = (Just n, y)
        go _ (x,y) = (x,y)

eval2 name (_,hm) = eval name hm

eval3 name (_,hm) = (show $ eval name hm) `deepseq` return ()

bprint :: a -> IO String
bprint a = do h <- walkHeap a
              return $ evalState (mprint b (snd $ h Map.! b)) (0,h)
  where b = asBox a

dprint :: a -> IO (String, HeapMap)
dprint a = do h <- walkHeap a
              return $ evalState (do
                s <- mprint b (snd $ h Map.! b)
                (_,h) <- get
                return (s,h)
                ) (0,h)
  where b = asBox a

-- This fromJust can fail because of garbage collection
-- That means some pointer to b inside the data structure has changed
mlp :: Box -> PrintState String
mlp b = do (_,h) <- get
           mprint b (snd $ (fromJust2 "1") $ Map.lookup b h)

setName :: Box -> PrintState ()
setName b = modify go
  where go (i,h) = (i + 1, Map.adjust (set i) b h)
        set i (Nothing, closure) = (Just ("t" ++ show i), closure)

getName :: Box -> PrintState (Maybe String)
getName b = do (_,h) <- get
               return $ fst $ (fromJust2 "2") $ Map.lookup b h


getCount :: PrintState Integer
getCount = do (i,_) <- get
              return i

getSetName :: Box -> PrintState String
getSetName b = do mn <- getName b
                  case mn of
                    Nothing   -> do setName b
                                    n <- getName b
                                    return $ fromJust2 "3" n
                    Just name -> return name

mbParens t@('"':_) = t
mbParens t@('(':_) = t
mbParens t | ' ' `elem` t = "(" ++ t ++ ")"
           | otherwise    = t

countReferences :: Box -> PrintState Int
countReferences b = do
  (_,h) <- get
  return $ sum $ map countR (Map.elems h)
 where countR (_,c) = length $ filter (== b) $ allPtrs c

mprint :: Box -> Closure -> PrintState String
mprint _ (ConsClosure (StgInfoTable _ _ _ _) _ [dataArg] _ modl name) =
 return $ case (modl, name) of
    k | k `elem` [ ("GHC.Word", "W#")
                 , ("GHC.Word", "W8#")
                 , ("GHC.Word", "W16#")
                 , ("GHC.Word", "W32#")
                 , ("GHC.Word", "W64#")
                 ] -> show dataArg

    k | k `elem` [ ("GHC.Integer.Type", "S#")
                 , ("GHC.Types", "I#")
                 , ("GHC.Int", "I8#")
                 , ("GHC.Int", "I16#")
                 , ("GHC.Int", "I32#")
                 , ("GHC.Int", "I64#")
                 ] -> show $ (fromIntegral :: Word -> Int) dataArg

    ("GHC.Types", "C#") -> show . chr $ fromIntegral dataArg

    ("Types", "D#") -> printf "%0.5f" (unsafeCoerce dataArg :: Double)
    ("Types", "F#") -> printf "%0.5f" (unsafeCoerce dataArg :: Double)

    c -> "Missing ConsClosure pattern for " ++ show c

-- Empty ByteStrings point to a nullForeignPtr, evaluating it leads to an
-- Prelude.undefined exception
mprint _ (ConsClosure (StgInfoTable 1 3 _ 0) _ [_,0,0] _ "Data.ByteString.Internal" "PS")
  = return "ByteString[0,0]()"

mprint _ (ConsClosure (StgInfoTable 1 3 _ 0) [bPtr] [_,start,end] _ "Data.ByteString.Internal" "PS")
  = do cPtr  <- mlp bPtr
       return $ printf "ByteString[%d,%d](%s)" start end cPtr

mprint _ (ConsClosure (StgInfoTable 2 3 _ 1) [bPtr1,bPtr2] [_,start,end] _ "Data.ByteString.Lazy.Internal" "Chunk")
  = do cPtr1 <- mlp bPtr1
       cPtr2 <- mlp bPtr2
       return $ printf "Chunk[%d,%d](%s,%s)" start end cPtr1 cPtr2

mprint _ (ConsClosure (StgInfoTable 1 0 CONSTR_1_0 _) [bPtr] [] _ _ name)
  = do cPtr <- mlp bPtr
       return $ name ++ " " ++ mbParens cPtr

mprint _ (ConsClosure (StgInfoTable 0 0 CONSTR_NOCAF_STATIC _) [] [] _ _ name)
  = return name

mprint _ (ConsClosure (StgInfoTable 0 _ CONSTR_NOCAF_STATIC _) [] args _ _ name)
  = return $ name ++ " " ++ show args

mprint b (ConsClosure (StgInfoTable 2 0 _ 1) [bHead,bTail] [] _ "GHC.Types" ":")
  = do b' <- getName b
       case b' of
         Just n  -> return n
         Nothing -> do i <- getCount
                       setName b
                       n <- liftM (fromJust2 "4") $ getName b
                       cHead <- liftM mbParens $ mlp bHead
                       cTail <- liftM mbParens $ mlp bTail
                       r <- countReferences b
                       return $ case r > 1 || (i == 0 && r == 1) of
                         True -> printf "%s=(%s:%s)" n cHead cTail
                         False -> printf "%s:%s" cHead cTail

mprint b (ConsClosure (StgInfoTable _ 0 _ _) bPtrs [] _ _ name)
  = do b' <- getName b
       case b' of
         Just n  -> return n
         Nothing -> do i <- getCount
                       setName b
                       n <- liftM (fromJust2 "5") $ getName b
                       cPtrs <- mapM ((liftM mbParens) . mlp) bPtrs
                       let tPtrs = intercalate " " cPtrs
                       r <- countReferences b
                       return $ case r > 1 || (i == 0 && r == 1) of
                         True -> printf "%s=(%s %s)" n name tPtrs
                         False -> printf "%s %s" name tPtrs


mprint _ (ArrWordsClosure (StgInfoTable 0 0 ARR_WORDS 0) bytes arrWords)
  = return $ intercalate "," (map (printf "0x%x") arrWords :: [String])

mprint _ (IndClosure (StgInfoTable 1 0 _ 0) b)
  = mlp b

-- Reversed order of ptrs
mprint b (ThunkClosure (StgInfoTable _ _ _ _) bPtrs args)
  = do name <- getSetName b
       cPtrs <- mapM mlp $ reverse bPtrs
       let tPtrs = intercalate "," cPtrs
       return $ case null args of
         True  -> name ++ "(" ++ tPtrs ++ ")"
         False -> name ++ show args ++ "(" ++ tPtrs ++ ")"

mprint b (FunClosure (StgInfoTable _ _ _ _) bPtrs args)
  = do name <- getSetName b
       cPtrs <- mapM mlp $ reverse bPtrs
       let tPtrs = intercalate "," cPtrs
       return $ case null args of
         True  -> name ++ "(" ++ tPtrs ++ ")"
         False -> name ++ show args ++ "(" ++ tPtrs ++ ")"

mprint b (APClosure (StgInfoTable 0 0 _ _) _ _ _ _)
  = getSetName b

mprint _ c = return $ "Missing pattern for " ++ show c

tseq (Box a) = seq a ()

data Foo = Foo Int Int Int Int

