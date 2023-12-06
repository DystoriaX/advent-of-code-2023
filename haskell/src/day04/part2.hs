import Debug.Trace
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
   in length matches

solve pile =
  let accumulator (copies, tot) e =
        let numMatches = solveOne e
            curCopy : rest = copies
            newCopies = (map (+ curCopy) . take numMatches) rest ++ drop numMatches rest
         in (newCopies, tot + curCopy)
      ones = [1 | _ <- pile]
      (_, tot) = foldl accumulator (ones, 0) pile
   in tot

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
