import Data.Function
import Debug.Trace
import System.IO (isEOF)

split c "" = []
split c s =
  case dropWhile (== c) s of
    "" -> []
    s' -> [w | w /= ""] ++ split c s''
      where
        (w, s'') = break (== c) s'

-- (Time, Distance)
type Race = (Int, Int)

countBetter :: Race -> Int
countBetter (t, d) =
  length $ filter (\h -> (t - h) * h > d) [0 .. t]

solve :: [Race] -> Int
solve races = product $ map countBetter races

parseLine = do
  str <- getLine
  let [_, values] = split ':' str
  return (map (read :: String -> Int) $ split ' ' values)

main = do
  time <- parseLine
  distance <- parseLine
  print (solve $ zip time distance)
