{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson as DA
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    decode,
    object,
    withObject,
    (.:),
    (.=),
  )
import Graph (adjList, bfs, checkList)
import Web.Scotty

data WordPair = WordPair
  { start :: String,
    end :: String
  }
  deriving (Eq, Show)

instance FromJSON WordPair where
  parseJSON = withObject "WordPair" $ \o ->
    WordPair
      <$> o .: "start"
      <*> o .: "end"

instance ToJSON WordPair where
  toJSON (WordPair first second) =
    object
      [ "start" .= first,
        "end" .= second
      ]

newtype ShortList = ShortList
  { wordList :: [String]
  }
  deriving (Eq, Show)


instance FromJSON ShortList where
  parseJSON = withObject "ShortList" $ \o ->
    ShortList <$> o .: "wordList"


main :: IO ()
main = scotty 3000 $ do
  {-
  curl -X POST -H "Content-Type: application/json" -d '{"start": "finks", "end": "slurs"}' http://localhost:3000/shortest-path
  
  -}
  post "/shortest-path" $ do
    wordFile <- liftIO $ readFile "./src/words.txt"
    let g = adjList $ lines wordFile
    requestBody <- body
    case decode requestBody of
      Just wordPair -> do
        let startWord = start wordPair
        let endWord = end wordPair

        let shortestPath = bfs g startWord endWord
        json $ object ["shortestPath" .= shortestPath]
      Nothing -> do
        json $ object ["error" .= ("Invalid JSON format" :: String)]

  {-
  curl -X POST -H "Content-Type: application/json" -d '{"wordList":["forty","porty","potty","potsy","popsy","poppy","poppa","zoppa","zoppo"]}' http://localhost:3000/check-path
  -}
  post "/check-path" $ do
    wordFile <- liftIO $ readFile "./src/words.txt"
    let g = adjList $ lines wordFile
    requestBody <- body
    case decode requestBody of
      Just parsedShortList -> do
        let shortList = wordList parsedShortList
        let shortestPath = bfs g (head shortList) (last shortList)
        let checkResult = checkList shortList shortestPath g
        json $ object ["checkPath" .= shortestPath, "checkResult" .= checkResult]
      Nothing -> do
        json $ object ["error" .= ("Invalid JSON format" :: String)]
