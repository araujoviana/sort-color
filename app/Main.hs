module Main where

import System.Environment
import Data.Char

type FolderPath = String
type Color = String
data Order = Asc | Desc deriving (Show, Read)

usage :: String
usage = "Usage: ./sort-color <folder path> <color> <order>"

main :: IO ()
main = do
  args <- getArgs
  case isolateArgs args of
    Right (f,c,o) -> putStrLn $ "Folder: " ++ f ++ ", Color: " ++ c ++ ", Order: " ++ show o
    Left err -> mapM_ putStrLn [err, usage]
  return ()


isolateArgs :: [String] -> Either String (String, String, Order)
isolateArgs [f,c,o] =
  let folder = map toLower f
      color = map toLower c
      order = map toLower o
  in case order of
    "asc" -> Right (folder, color, Asc)
    "desc" -> Right (folder, color, Desc)
    _ -> Left "Invalid order"
isolateArgs _ = Left "Invalid number of arguments"
