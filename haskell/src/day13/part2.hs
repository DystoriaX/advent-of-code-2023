import Data.List
import Debug.Trace
import System.IO (isEOF)

type Grid = [[Char]]

getHorizontal :: Grid -> [Int]
getHorizontal grid =
  let isMirror grid n = numDiff == 1
        where
          firstHalf = take n grid
          secondHalf = drop n grid
          getDiff xs ys = sum $ zipWith (\x y -> if x == y then 0 else 1) xs ys
          numDiff = sum $ zipWith getDiff (reverse firstHalf) secondHalf
   in filter (isMirror grid) [1 .. length grid - 1]

solveAtomic :: Grid -> Int
solveAtomic grid = trace (show (getHorizontal grid, getHorizontal $ transpose grid)) $ 100 * h + v
  where
    h = sum $ getHorizontal grid
    v = sum $ getHorizontal (transpose grid)

solve :: [Grid] -> Int
solve grids = sum $ map solveAtomic grids

getGrid = do
  done <- isEOF
  if done
    then return []
    else do
      line <- getLine
      if line == ""
        then do
          return []
        else do
          rest <- getGrid
          return (line : rest)

getGrids = do
  done <- isEOF
  if done
    then return []
    else do
      grid <- getGrid
      rest <- getGrids
      return (grid : rest)

main = do
  input <- getGrids
  print $ solve input
