{-# LANGUAGE LambdaCase #-}

import Data.List
import Data.Maybe

solve (moves, adjList) =
  let posS = scanl (\pos move -> move (fromJust $ lookup pos adjList)) "AAA" (cycle moves)
   in elemIndex "ZZZ" posS

parseMove = map (\case 'L' -> fst; 'R' -> snd)

parseNodes str =
  let parseLine [from, _, _ : l, r] = (from, (init l, init r))
   in map (parseLine . words) str

parseInput str =
  let moveStr : _ : nodes = lines str
   in (parseMove moveStr, parseNodes nodes)

main = do
  content <- getContents
  let input = parseInput content
  print $ solve input
