import Data.Function
import Data.List
import Data.Maybe
import System.IO (isEOF)

split c "" = []
split c s =
  case dropWhile (== c) s of
    "" -> []
    s' -> [w | w /= ""] ++ split c s''
      where
        (w, s'') = break (== c) s'

type Map = [(Int, Int, Int)]

locOf x mp =
  case find (\(dest, src, len) -> src <= x && x < src + len) mp of
    Nothing -> x
    Just (dest, src, len) -> dest + x - src

solve :: ([Int], [Map]) -> Int
solve (seeds, maps) =
  let accumulator acc mp = map (`locOf` mp) acc
   in minimum $ foldl accumulator seeds maps

readSeed = do
  seedRaw <- getLine
  ignore <- getLine
  let [_, seedStr] = split ':' seedRaw
  return ((map (read :: String -> Int) . split ' ') seedStr)

readMap = do
  done <- isEOF
  if done
    then return []
    else do
      mp <- getLine
      if mp == ""
        then return []
        else do
          let toTuple [x, y, z] = (x, y, z)
          let parsedData = toTuple $ (map (read :: String -> Int) . split ' ') mp
          rest <- readMap
          return (parsedData : rest)

readMaps = do
  done <- isEOF
  if done
    then return []
    else do
      name <- getLine
      map <- readMap
      rest <- readMaps
      return (map : rest)

main = do
  seeds <- readSeed
  maps <- readMaps
  print $ solve (seeds, maps)
