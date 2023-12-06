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

goodGame = [("red", 12), ("green", 13), ("blue", 14)]

isGoodSet = all (\(ball, cnt) -> cnt <= fromJust (lookup ball goodGame))

solve games = sum [id | (id, sets) <- games, all isGoodSet sets]

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
