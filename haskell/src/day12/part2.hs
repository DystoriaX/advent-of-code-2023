import Data.Array
import Data.List
import Debug.Trace

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

solveAtomic :: (String, [Int]) -> Int
solveAtomic (m, conf) = d n (k + 1) 0 + d n k (conf' ! k)
  where
    um = intercalate "?" (replicate 5 m)
    uconf = concat $ replicate 5 conf
    -- um = m
    -- uconf = conf
    n = length um
    k = length uconf
    m' = listArray (1, n) um
    conf' = listArray (0, k) (0 : uconf) -- add prefix 0 for convenience

    -- d i j c:
    --   # of arrangements of first i prefix of spring and first j prefix of
    --   intended configuration, where there are c contiguous # so far
    d 0 1 0 = 1
    d 0 j c = 0
    d i 0 c = 0
    d i j c =
      let valHash =
            if c == 0 || j > k || c > (conf' ! j)
              then 0
              else ds ! (i - 1, j, c - 1)
          valDot =
            if c /= 0
              then 0
              else ds ! (i - 1, j, 0) + ds ! (i - 1, j - 1, conf' ! (j - 1))
       in case m' ! i of
            '#' -> valHash
            '.' -> valDot
            _ -> valHash + valDot
    ds = listArray bounds [d i j c | (i, j, c) <- range bounds]
    bounds = ((0, 0, 0), (n, k + 1, n))

solve xs =
  sum $ map solveAtomic xs

main = do
  inStr <- lines <$> getContents
  print $ solve (map parseInput inStr)
