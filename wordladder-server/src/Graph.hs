module Graph where

type Graph = [(String, [String])]
-- [("amend",["emend","amens","abend"])]

trialWords :: IO ()
trialWords = do 
    wordFile <- readFile "./src/words.txt"
    let g = adjList $ lines wordFile
    let result = bfs g "finks" "grope"
    print result

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
neighbours word wordList = filter (\w -> isLetterOff w word) wordList

adjList :: [String] -> Graph
adjList wordList = [(word, neighbours word wordList) | word <- wordList]

bfs :: Graph -> String -> String -> Maybe [String]
bfs graph start end = go [(start, [start])] []
  where
    go [] _ = Nothing
    go ((node, path):queue) visited
      | node == end = Just (reverse path)
      | node `elem` visited = go queue visited
      | otherwise = go (queue ++ neighbors) (node:visited)
        where
          neighbors = case lookup node graph of
            Just ns -> [(n, n:path) | n <- ns]
            Nothing -> []

 