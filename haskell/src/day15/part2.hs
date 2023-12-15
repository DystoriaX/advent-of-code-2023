import Data.Char
import Data.List
import Data.Map qualified as Map
import Debug.Trace

split c "" = []
split c s =
  case dropWhile (== c) s of
    "" -> []
    s' -> [w | w /= ""] ++ split c s''
      where
        (w, s'') = break (== c) s'

data Op = Add (String, Int) | Remove String

hashOf str =
  let f' "" v = v
      f' (c : cs) v = f' cs (((v + ord c) * 17) `mod` 256)
   in f' str 0

type Book = Map.Map Int [(String, Int)]

focusingPowerOf :: Book -> Int
focusingPowerOf book =
  let powerOf box = sum $ zipWith (*) [1 ..] (map snd box)
   in sum $ map (\(boxId, box) -> (boxId + 1) * powerOf box) (Map.assocs book)

updateBox k v [] = [(k, v)]
updateBox k v ((k', v') : xs) =
  if k' == k then (k, v) : xs else (k', v') : updateBox k v xs

removeBox k [] = []
removeBox k ((k', v') : xs) =
  if k' == k then xs else (k', v') : removeBox k xs

solve :: [Op] -> Int
solve xs = focusingPowerOf $ foldl accum (Map.empty :: Book) xs
  where
    accum book elem =
      case elem of
        Add (k, f) -> add book k f
        Remove k -> remove book k
    add book k v =
      let hash = hashOf k
       in case Map.lookup hash book of
            Nothing -> Map.insert hash [(k, v)] book
            Just l -> Map.insert hash (updateBox k v l) book
    remove book k =
      let hash = hashOf k
       in case Map.lookup hash book of
            Nothing -> book
            Just l -> Map.insert hash (removeBox k l) book

parseInput str =
  case last str of
    '-' -> Remove (init str)
    f -> Add (init $ init str, read [f] :: Int)

main = do
  input <- getLine
  let seq = map parseInput $ split ',' input
  print $ solve seq
