import System.IO (isEOF)

split c "" = []
split c s =
  let (w, s') = break (== c) s
   in case s' of
        "" -> [w | w /= ""]
        _ : s'' -> [w | w /= ""] ++ split c s''

parseInput str =
  let [_, cardsStr] = split ':' str
      [winStr, myStr] = split '|' cardsStr
      toIntList = map (read :: String -> Int) . split ' '
   in (toIntList winStr, toIntList myStr)

solveOne (winCards, myCards) =
  let matches = filter (`elem` winCards) myCards
   in if null matches then 0 else 2 ^ (length matches - 1)

solve pile =
  sum $ map solveOne pile

getInput = do
  done <- isEOF
  if done
    then return []
    else do
      line <- getLine
      rest <- getInput
      return (line : rest)

main = do
  input <- getInput
  print (solve $ map parseInput input)
