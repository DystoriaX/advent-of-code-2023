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

type Map = [(Int, Int, Int)]

locOf :: (Int, Int) -> Map -> [(Int, Int)]
locOf seed@(sl, sr) [] = [seed | sl <= sr]
locOf seed@(sl, sr) ((dest, kl, kr) : rest) =
  if sl > sr
    then []
    else
      let intL = max sl kl
          intR = min sr kr
          newInt = [(intL - kl + dest, intR - kl + dest) | intL <= intR]
          outL = (sl, min (kl - 1) sr)
          outR = (max (kr + 1) sl, sr)
       in newInt ++ locOf outL rest ++ locOf outR rest

solve (seeds, maps) =
  let seeds' = map (\(l, len) -> (l, l + len - 1)) seeds
      maps' = map (map (\(dest, l, len) -> (dest, l, l + len - 1))) maps
      accumulator acc elem = concatMap (`locOf` elem) acc
   in minimum $ map fst $ foldl accumulator seeds' maps'

parseSeed str =
  let ss = split ' ' str
      helper [] = []
      helper (start : len : rest) = (read start :: Int, read len :: Int) : helper rest
   in helper ss

readSeed = do
  seedRaw <- getLine
  ignore <- getLine
  let [_, seedStr] = split ':' seedRaw
  return (parseSeed seedStr)

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
