split c "" = []
split c s =
  case dropWhile (== c) s of
    "" -> []
    s' -> [w | w /= ""] ++ split c s''
      where
        (w, s'') = break (== c) s'

parseInput = map (map (read :: String -> Int) . split ' ')

solveAtomic h =
  let getSteps [] = []
      getSteps xs@(x : xs') =
        if all (== 0) xs then [xs] else xs : getSteps diffs
        where
          diffs = zipWith (-) xs' xs
      steps = getSteps (reverse h)
      prediction = foldr (\e acc -> last e + acc) 0 steps
   in prediction

solve histories =
  sum $ map solveAtomic histories

main = do
  input <- lines <$> getContents
  print $ solve (parseInput input)
