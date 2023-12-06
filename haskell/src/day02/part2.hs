import Data.Function
import Data.Maybe
import System.IO (isEOF)

split c "" = []
split c s =
  let (w, s') = break (== c) s
   in case s' of
        "" -> [w | w /= ""]
        _ : s'' -> [w | w /= ""] ++ split c s''

parseGame str =
  let parseSet str = str & split ',' & map ((\[cntStr, ball] -> (ball, read cntStr :: Int)) . split ' ')
      [gameStr, setStr] = split ':' str
      [_, gameIdS] = split ' ' gameStr
      gameId = read gameIdS :: Int
      sets = split ';' setStr
   in (gameId, map parseSet sets)

powerOfSets sets =
  let combineSets acc s = map (\(ball, cnt) -> (ball, max cnt $ fromMaybe 0 $ lookup ball s)) acc
      initSet = [("red", 0), ("green", 0), ("blue", 0)]
      minBalls = foldl combineSets initSet sets
   in product [cnt | (ball, cnt) <- minBalls]

solve games = sum [powerOfSets sets | (_, sets) <- games]

readline = do
  done <- isEOF
  if done
    then return []
    else do
      game <- getLine
      rest <- readline
      return (game : rest)

main = do
  games <- readline
  print (solve $ map parseGame games)
