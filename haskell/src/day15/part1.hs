import Data.Char

split c "" = []
split c s =
  case dropWhile (== c) s of
    "" -> []
    s' -> [w | w /= ""] ++ split c s''
      where
        (w, s'') = break (== c) s'

solveAtomic str =
  let f' "" v = v
      f' (c : cs) v = f' cs (((v + ord c) * 17) `mod` 256)
   in f' str 0

solve xs =
  sum $ map solveAtomic xs

main = do
  input <- getLine
  let seq = split ',' input
  print $ solve seq
