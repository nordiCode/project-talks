{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graph ( adjList, bfs ) 
import Web.Scotty 
import Control.Monad.IO.Class (liftIO) 
import Data.Aeson as DA
    ( decode,
      ToJSON(toJSON),
      FromJSON(parseJSON),
      (.:),
      withObject,
      KeyValue((.=)),
      object ,  (.=))

data WordPair = WordPair
    { start :: String
    , end :: String
    } deriving (Eq, Show)

instance FromJSON WordPair where
    parseJSON = withObject "WordPair" $ \o ->
        WordPair <$> o .: "start"
                 <*> o .: "end"

instance ToJSON WordPair where
    toJSON (WordPair first second) =
        object [ "start" .= first
               , "end" .= second
               ]

main :: IO ()
main = scotty 3000 $ do
  post "/shortest-path" $ do
    wordFile <- liftIO $ readFile "./src/words.txt"
    let g = adjList $ lines wordFile
    requestBody <- body
    case decode requestBody of
        Just wordPair  -> do
            let startWord = start wordPair
                endWord = end wordPair
            
            let shortestPath = bfs g startWord endWord
            json $ object ["shortestPath" .= shortestPath]
        Nothing -> do
            json $ object ["error" .= ("Invalid JSON format" :: String)]
