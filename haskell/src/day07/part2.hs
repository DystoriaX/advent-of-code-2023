import Data.Function
import Data.List
import Data.Maybe
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
  = J
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | T
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

costOf [] [] = Just 0
costOf [] destList = Just (sum destList)
costOf srcList [] = Nothing
costOf (s : srcList) (d : destList) =
  case costOf srcList destList of
    Nothing -> Nothing
    Just x -> if s <= d then Just (x + d - s) else Nothing

freqCards =
  [ ([5], FiveOfAKind),
    ([4, 1], FourOfAKind),
    ([3, 2], FullHouse),
    ([3, 1, 1], ThreeOfAKind),
    ([2, 2, 1], TwoPair),
    ([2, 1, 1, 1], OnePair),
    ([1, 1, 1, 1, 1], HighCard)
  ]

setOf cards =
  let (normal, joker) = partition (/= J) cards
      distinct = nub normal
      freq = sortBy (flip compare) $ map (\x -> length $ filter (== x) cards) distinct
      jokerCnt = length joker
      filterCards (conf, _) = case costOf freq conf of
        Nothing -> False
        Just x -> x <= jokerCnt
   in snd $ fromJust $ find filterCards freqCards

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
