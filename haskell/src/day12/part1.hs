split c "" = []
split c s =
  case dropWhile (== c) s of
    "" -> []
    s' -> [w | w /= ""] ++ split c s''
      where
        (w, s'') = break (== c) s'

parseInput :: String -> (String, [Int])
parseInput s =
  let [mp, confStr] = split ' ' s
      conf = map (read :: String -> Int) $ split ',' confStr
   in (mp, conf)

solveAtomic (m, conf) =
  let gen [] = [[]]
      gen (x : xs) =
        let ret = gen xs
         in if x == '?'
              then map ('#' :) ret ++ map ('.' :) ret
              else map (x :) ret
      getConf m =
        let helper [] cnt = [cnt]
            helper (x : xs) cnt =
              if x == '#'
                then helper xs (cnt + 1)
                else cnt : helper xs 0
         in filter (/= 0) $ helper m 0
   in length $ filter (== conf) $ map getConf (gen m)

solve xs =
  sum $ map solveAtomic xs

main = do
  inStr <- lines <$> getContents
  print $ solve (map parseInput inStr)
