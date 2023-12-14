import Data.List
import Debug.Trace
import System.IO (isEOF)

type Grid = [[Char]]

getHorizontal :: Grid -> [Int]
getHorizontal grid =
  let isMirror grid n = and $ zipWith (==) (reverse firstHalf) secondHalf
        where
          firstHalf = take n grid
          secondHalf = drop n grid
   in filter (isMirror grid) [1 .. length grid - 1]

solveAtomic :: Grid -> Int
solveAtomic grid = 100 * h + v
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
