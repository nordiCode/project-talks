module Graph where

type Graph = [(String, [String])]

-- let g = [("amend",["emend","amens","abend"])]

{-
  Compares two strings letter by letter outputting a bool on
  whether they match. Check if the length is one because that
  shows there is a single letter difference.
-}
isLetterOff :: String -> String -> Bool
isLetterOff w1 w2 = length (filter not (zipWith (==) w1 w2)) == 1

{-
  Compares all words in the wordlist to the word outputting
  those off by a letter.
-}
neighbours :: String -> [String] -> [String]
neighbours word = filter (`isLetterOff` word)

adjList :: [String] -> Graph
adjList wordList = [(word, neighbours word wordList) | word <- wordList]

bfs :: Graph -> String -> String -> Maybe [String]
bfs graph start end = go [(start, [start])] []
  where
    go [] _ = Nothing
    go ((node, path) : queue) visited
      | node == end = Just (reverse path)
      | node `elem` visited = go queue visited
      | otherwise = go (queue ++ neighbors) (node : visited)
      where
        neighbors = case lookup node graph of
          Just ns -> [(n, n : path) | n <- ns]
          Nothing -> []

checkList :: [String] -> Maybe [String] -> Graph -> Bool
checkList xs (Just sp) graph = all (`elem` graphKeys) xs && length xs == length sp
  where
    graphKeys = map fst graph
checkList _ Nothing _ = False
