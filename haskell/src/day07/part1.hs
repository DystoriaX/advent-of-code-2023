import Data.Function
import Data.List
import Debug.Trace
import System.IO (isEOF)

split c "" = []
split c s =
  case dropWhile (== c) s of
    "" -> []
    s' -> [w | w /= ""] ++ split c s''
      where
        (w, s'') = break (== c) s'

data Card
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | T
  | J
  | Q
  | K
  | A
  deriving (Eq, Show, Ord)

data Set
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving (Eq, Show, Ord)

cardOf '2' = Two
cardOf '3' = Three
cardOf '4' = Four
cardOf '5' = Five
cardOf '6' = Six
cardOf '7' = Seven
cardOf '8' = Eight
cardOf '9' = Nine
cardOf 'T' = T
cardOf 'J' = J
cardOf 'Q' = Q
cardOf 'K' = K
cardOf 'A' = A

setOf cards =
  let distinct = nub cards
      freq = map (\x -> length $ filter (== x) cards) distinct
   in case sortBy (flip compare) freq of
        [5] -> FiveOfAKind
        [4, 1] -> FourOfAKind
        [3, 2] -> FullHouse
        [3, 1, 1] -> ThreeOfAKind
        [2, 2, 1] -> TwoPair
        [2, 1, 1, 1] -> OnePair
        [1, 1, 1, 1, 1] -> HighCard

solve handBidList =
  let sortedBid = map snd $ sortOn (\(cards, _) -> (setOf cards, cards)) handBidList
   in sum $ zipWith (*) sortedBid [1 .. (length sortedBid)]

readHandAndBid = do
  done <- isEOF
  if done
    then return []
    else do
      str <- getLine
      rest <- readHandAndBid
      let [cards, bid] = split ' ' str
      return ((map cardOf cards, read bid :: Int) : rest)

main = do
  input <- readHandAndBid
  print $ solve input
