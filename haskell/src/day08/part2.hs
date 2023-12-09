{-# LANGUAGE LambdaCase #-}

import Data.List
import Data.Maybe

solve (moves, adjList) =
  let startNodes = map fst $ filter (\(node, _) -> last node == 'A') adjList
      moveStreamOf node = scanl (\pos move -> move (fromJust $ lookup pos adjList)) node (cycle moves)
   in foldl lcm 1 $ map (fromJust . findIndex (\node -> last node == 'Z') . moveStreamOf) startNodes

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
