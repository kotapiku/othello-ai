{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
module Play where

import           Control.Monad
import           Control.Monad.Loops (allM)
import           Data.Array
import           Data.Array.IO
import           Data.Array.MArray
import           Data.Either
import           Data.Foldable       (foldrM)
import           Data.List           (maximumBy, splitAt, (\\))
import           Data.Tuple          (swap)

import           Color
import           Command
import           System.Random


-- 番兵付きの10x10配列
type Board = IOUArray (Int,Int) Int

initBoard :: IO Board
initBoard =
    do board <- newArray ( (0,0), (9,9) ) none
       mapM_ (\i ->
                  do writeArray board (i,0) sentinel
                     writeArray board (i,9) sentinel
                     writeArray board (0,i) sentinel
                     writeArray board (9,i) sentinel) [0..9]
       writeArray board (4,4) white
       writeArray board (4,5) black
       writeArray board (5,4) black
       writeArray board (5,5) white
       return board

isValidMove :: Board -> Color -> (Int,Int) -> IO Bool
isValidMove board color (i,j) =
    do e <- readArray board (i,j)
       if e == none then
           isEffective board color (i,j)
       else
           return False
-- 8方向
dirs = [ (i,j) | i <- [1,0,-1], j <- [1,0,-1] ] \\ [(0,0)]

-- 石を置いたときに、ひっくり返せるかどうか
isEffective :: Board -> Color -> (Int,Int) -> IO Bool
isEffective board color (i,j) =
    do ms <- flippableIndices board color (i,j)
       return $ not $ null ms

-- 石を置いたときにひっくり返えるところ
flippableIndices :: Board -> Color -> (Int,Int) -> IO [(Int,Int)]
flippableIndices board color (i,j) =
    do bs <- mapM (\(di,dj) -> flippableIndicesLine board color (di,dj) (i+di,j+dj)) dirs
       return $ concat bs

flippableIndicesLine board color (di,dj) (i,j) =
    checkLine (di,dj) (i,j) []
    where
      ocolor = oppositeColor color
      checkLine (di,dj) (i,j) r =
          do c <- readArray board (i,j)
             if c == ocolor then
                 checkLine' (di,dj) (i+di,j+dj) ( (i,j) : r )
             else
                 return []
      checkLine' (di,dj) (i,j) r =
          do c <- readArray board (i,j)
             if c == ocolor then
                 checkLine' (di,dj) (i+di,j+dj) ( (i,j) : r )
             else if c == color then
                      return r
                  else
                      return []

{-
   boardをそのまま返すのは一般には危険。
   だが、今のdoMoveの使用方法ならば問題ない。
-}
doMove :: Board -> Mv -> Color -> IO Board
doMove board GiveUp  color = return board
doMove board Pass    color = return board
doMove board (M i j) color =
    do ms <- flippableIndices board color (i,j)
       mapM_ (\(ii,jj) -> writeArray board (ii,jj) color) ms
       writeArray board (i,j) color
       return board

noIOdoMove :: Board -> Mv -> Color -> IO Board
noIOdoMove board GiveUp  color = return board
noIOdoMove board Pass    color = return board
noIOdoMove board (M i j) color =
    do ms <- flippableIndices board color (i,j)
       board' <- foldrM (\(ii,jj) acc -> write acc (ii,jj) color) board ms
       write board' (i,j) color

write :: Board -> (Int, Int) -> Int -> IO Board
write board (i, j) color =
    do (l1, l2) <- splitAt (i * 10 + j) <$> getElems board
       newListArray ( (0, 0), (9, 9) ) $ l1 ++ (color : tail l2)

-- 合法手
validMoves :: Board -> Color -> IO [ (Int,Int) ]
validMoves board color =
     filterM (isValidMove board color)
             [ (i,j) | i <- [1..8], j <- [1..8]]

play :: Board -> Color -> IO Mv
play board color =
    do ms <- validMoves board color
       zeronum <- count board none
       case ms of
         [] -> return Pass
         _  | zeronum > 12 -> playFirst board color ms
            | zeronum <= 12 -> playLast board color ms

-- 序盤
playFirst :: Board -> Color -> [ (Int, Int) ] -> IO Mv
playFirst board color ms =
    case l1 of
      [] -> do l2 <- filterM (fixedStone board color) ms
               case l2 of
                 [] -> uncurry M . fst <$> foldrM (\e (e1, acc) ->
                         case acc of
                           Left x -> do aa <- ((*) (-1)) <$> (noIOdoMove board (uncurry M e) color >>= negaAlpha color 6 (-1000) (-x))
                                        if | aa > x -> return (e, Left aa)
                                           | otherwise -> return (e1, acc)
                           Right _ -> return (e1, acc)) ((0, 0), Left (-1000)) ms
                 _ -> do k <- getStdRandom $ randomR (0, length l2-1)  -- 確定石があるなら取る
                         return $ uncurry M (l2 !! k)
      _ -> do k <- getStdRandom $ randomR (0, length l1-1)   -- 角が取れるなら取る
              return $ uncurry M (l1 !! k)
 where l1 = filter (\(i, j) -> (i == 1 || i == 8) && (j == 1 || j == 8)) ms

-- 終盤 読み切り
playLast :: Board -> Color -> [ (Int, Int) ] -> IO Mv
playLast board color ms = uncurry M . fst <$> foldrM (\e (e1, acc) ->
                            case acc of
                              Left x -> do aa <- ((*) (-1)) <$> (noIOdoMove board (uncurry M e) color >>= negaAlpha2 color False (-1000) (-x))
                                           if | aa > x -> return (e, Left aa)
                                              | otherwise -> return (e1, acc)
                              Right _ -> return (e1, acc)) ((0, 0), Left (-1000)) ms

-- 石の位置による評価
boardEvalArray = listArray ((0, 0), (9, 9))
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 30, -12, 0, -1, -1, 0, -12, 30, 0,
                 0, -12, -15, -3, -3, -3, -3, -15, -12, 0,
                 0, 0, -3, 0, -1, -1, 0, -3, 0, 0,
                 0, -1, -3, -1, -1, -1, -1, -3, -1, 0,
                 0, -1, -3, -1, -1, -1, -1, -3, -1, 0,
                 0, 0, -3, 0, -1, -1, 0, -3, 0, 0,
                 0, -12, -15, -3, -3, -3, -3, -15, -12, 0,
                 0, 30, -12, 0, -1, -1, 0, -12, 30, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

-- 評価関数
evaluate :: Board -> Color -> IO Int
evaluate board color =
    (+) <$> foldrM (\index acc ->
              do e <- readArray board index
                 case e of
                   _ | e == color -> return $ acc + (boardEvalArray ! index)
                     | e == oppositeColor color -> return $ acc - (boardEvalArray ! index)
                     | otherwise -> return acc)
        0 [ (i, j) | i <- [1 .. 8], j <- [1 .. 8] ]
       <*> validSpaceEval board color

-- 着手可能なマスの数
validSpaceEval :: Board -> Color -> IO Int
validSpaceEval board color =
    (-) <$> (length <$> validMoves board color)
        <*> (length <$> validMoves board (oppositeColor color))

-- 確定石
fixedStone :: Board -> Color -> (Int, Int) -> IO Bool
fixedStone board color (i, j)
    | (i == 1 || i == 8) && (j /= 1 && j /= 8) =
        (||) <$> allM (fmap (== color) . readArray board)
                     [(i, k) | k <- [1 .. j - 1]]
             <*> allM (fmap (== color) . readArray board)
                     [(i, k) | k <- [j + 1 .. 8]]
    | (j == 1 || j == 8) && (i /= 1 && i /= 8) =
        (||) <$> allM (fmap (== color) . readArray board)
                     [(k, j) | k <- [1 .. i - 1]]
             <*> allM (fmap (== color) . readArray board)
                     [(k, j) | k <- [i + 1 .. 8]]
    | (i == 2 || i == 7) && (j == 2 || j == 7) =
        (||) <$> allM (fmap (== color) . readArray board) (f1 (i, j))
             <*> allM (fmap (== color) . readArray board) (f2 (i, j))
    | otherwise = return False
 where f1 :: (Int, Int) -> [ (Int, Int) ]
       f1 (2, 2) = [(1, 1), (1, 2), (1, 3), (2, 1)]
       f1 (2, 7) = [(1, 8), (1, 7), (1, 6), (2, 8)]
       f1 (7, 2) = map swap $ f1 (2, 7)
       f1 (7, 7) = [(8, 8), (8, 7), (8, 6), (7, 8)]
       f2 :: (Int, Int) -> [ (Int, Int) ]
       f2 = map swap . f1

negaAlpha :: Color -> Int -> Int -> Int -> Board -> IO Int
negaAlpha color depth a b board
  | depth == 0 = if bool then evaluate board color else ((*) (-1)) <$> evaluate board color
  | otherwise = do 
      ms <- validMoves board nowcolor
      if | null ms -> negaAlpha color (depth-1) (-b) (-a) board
         | otherwise -> either id id <$> foldrM (\e acc -> case acc of
                                            Left x -> do aa <- (max x) . ((*) (-1)) <$> (noIOdoMove board (uncurry M e) nowcolor >>= negaAlpha color (depth-1) (-b) (-x))
                                                         if | aa >= b -> return $ Right aa
                                                            | otherwise -> return $ Left aa
                                            Right _ -> return acc) (Left a) ms
 where nowcolor = if bool then color else oppositeColor color -- 打つ人
       bool = depth `mod` 2 /= 0

negaAlpha2 :: Color -> Bool -> Int -> Int -> Board -> IO Int
negaAlpha2 color bool a b board =
    do isFin <- (== 0) <$> count board none
       ms <- validMoves board nowcolor
       ms' <- validMoves board (oppositeColor nowcolor)
       if | (isFin || (null ms && null ms')) -> if bool then count board color else ((*) (-1)) <$> count board color
          | null ms -> negaAlpha2 color (not bool) (-b) (-a) board
          | otherwise -> either id id <$> foldrM (\e acc -> case acc of
                                            Left x -> do aa <- (max x) . ((*) (-1)) <$> (noIOdoMove board (uncurry M e) nowcolor >>= negaAlpha2 color (not bool) (-b) (-x))
                                                         if | aa >= b -> return $ Right aa
                                                            | otherwise -> return $ Left aa
                                            Right _ -> return acc) (Left a) ms
 where nowcolor = if bool then color else oppositeColor color -- 打つ人

-- 石の数
count :: Board -> Color -> IO Int
count board color =
    do is <- filterM (\i ->
                          do e <- readArray board i
                             return $ e == color)
                     [ (i,j) | i <- [1..8], j <- [1..8]]
       return $ length is


-- 盤面の出力
putBoard :: Board -> IO ()
putBoard board =
    do putStrLn " |A B C D E F G H "
       putStrLn "-+----------------"
       mapM_ putBoardLine [1..8]
       putStrLn "  (X: Black,  O: White)"
    where
      putC c | c == none  = putStr " "
             | c == white = putStr "O"
             | c == black = putStr "X"
      putBoardLine j =
          do putStr $ show j ++ "|"
             mapM_ (\i -> do e <- readArray board (i,j)
                             putC e >> putStr " ") [1..8]
             putStrLn ""

