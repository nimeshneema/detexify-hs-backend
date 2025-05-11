{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (when, guard)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM
import Control.Exception
import System.IO.Error
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.Aeson as JSON

import Classifier
import StrokeSample
import Strokes
import JSON.Strokes
import JSON.Results

import Web.Scotty
import Network.HTTP.Types (badRequest400)

import Data.Either (isRight)
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

-- Server configuration
port :: Int
port = 3000

cK :: Int
cK = 50

alpha :: Double
alpha = 2 * pi * 15 / 360

snapshotJsonFile :: FilePath
snapshotJsonFile = "snapshot.json"

-- Stroke preprocessing pipeline
sanitize :: Strokes -> Strokes
sanitize = (map (dominant alpha
                . unduplicate
                . redistribute 10
                . aspectrefit (Point (0,0), Point (1,1))
                . smooth
                . unduplicate)
                ) . limit 10

process :: Strokes -> Strokes
process = sanitize

-- Validate strokes
validate :: Either String Strokes -> Either String Strokes
validate (Left s) = Left s
validate (Right s)
  | not (null s) && all (not . null) s = Right s
  | otherwise = Left "Illegal stroke."

-- Error and message responses
jsonerror :: String -> ActionM ()
jsonerror e = do
  status badRequest400
  json $ JSON.object [("error", JSON.toJSON e)]

jsonmessage :: String -> ActionM ()
jsonmessage m = json $ JSON.object [("message", JSON.toJSON m)]

-- Handler for classification
classifyHandler :: Classifier StrokeSample -> Either String Strokes -> ActionM ()
classifyHandler c input =
  case input of
    Left err -> jsonerror err
    Right strokes -> do
      result <- liftIO $ classifyWithClassifier c (newStrokeSample (process strokes))
      json result

-- Handler for training
trainHandler :: Classifier StrokeSample -> Either String Strokes -> String -> ActionM ()
trainHandler c input charId =
  case input of
    Left err -> jsonerror err
    Right strokes -> do
      let processed = process strokes
      liftIO $ print (show processed)
      liftIO $ trainClassifier c charId (newStrokeSample processed)
      jsonmessage "Sample was successfully trained."

-- Snapshot persistence
snapshot :: Classifier StrokeSample -> IO ()
snapshot c = do
  s <- atomically $ readTVar $ samples c
  BL.writeFile snapshotJsonFile $ JSON.encode s

load :: Classifier StrokeSample -> IO ()
load c = do
  jsonString <- BL.readFile snapshotJsonFile
  case JSON.decode jsonString of
    Nothing -> return ()
    Just snapshot -> atomically $ writeTVar (samples c) snapshot

loadSuccess :: Classifier StrokeSample -> IO Bool
loadSuccess c = load c >> return True `catch` handle
  where
    handle :: IOError -> IO Bool
    handle e
      | isDoesNotExistError e = return False
      | otherwise = ioError e

-- Main function
main :: IO ()
main = do
  putStrLn "hs-classifier at http://localhost:3000"
  c <- newClassifier cK

  loaded <- loadSuccess c
  putStrLn $ if loaded then "Snapshot loaded." else "No snapshot found."

  enableSnapshot <- tryJust (guard . isDoesNotExistError) $ getEnv "ENABLESNAPSHOT"

  scotty port $ do
    get "/" $ do
      json $ JSON.object [("server", "Nöt Betty :("), ("version", "0.0.2")]

    post "/classify" $ do
      d <- body
      let input = validate $ JSON.eitherDecode d
      classifyHandler c input

    post "/train/:id" $ do
      charId <- param "id"
      d <- body
      let input = validate $ JSON.eitherDecode d
      trainHandler c input charId

    when (isRight enableSnapshot) $ do
      post "/save-snapshot" $ do
        liftIO $ snapshot c
        jsonmessage "Snapshotted."

      post "/load-snapshot" $ do
        success <- liftIO $ loadSuccess c
        jsonmessage $ if success then "Loaded" else "No. Just no."
