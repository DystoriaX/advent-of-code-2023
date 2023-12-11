import Data.List
import Debug.Trace
import System.IO (isEOF)

split c "" = []
split c s =
  case dropWhile (== c) s of
    "" -> []
    s' -> [w | w /= ""] ++ split c s''
      where
        (w, s'') = break (== c) s'

enumerate = zip [0 ..]

type Grid = [[Char]]

solve :: Grid -> Int
solve grid =
  let rows = findIndices (all (== '.')) grid
      columns = findIndices (all (== '.')) (transpose grid)
      positions = concatMap (\(idx, row) -> zip (repeat idx) (elemIndices '#' row)) (enumerate grid)
      getDistance (r1, c1) (r2, c2) =
        let minR = min r1 r2
            maxR = max r1 r2
            minC = min c1 c2
            maxC = max c1 c2
         in (length $ intersect rows [minR .. maxR]) + (length $ intersect columns [minC .. maxC]) + (maxR - minR) + (maxC - minC)
   in (`div` 2) $ sum $ [getDistance x y | x <- positions, y <- positions, x /= y]

main = do
  grid <- lines <$> getContents
  print $ solve grid
