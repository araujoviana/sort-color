module Main where

import System.Environment
import System.Directory
import System.FilePath
import Data.Char
import Control.Monad
import Data.Either

type FolderPath = String
type Color = String
data Order = Asc | Desc deriving (Show, Read)

usage :: String
usage = "Usage: ./sort-color <folder path> <color> <order>"

main :: IO ()
main = do
  args <- getArgs

  when (isLeft $ isolateArgs args) $ mapM_ putStrLn ["Invalid arguments", usage]
  let Right (folder, color, order) = isolateArgs args

  -- Select all the files in the folder that have the .bmp extension
  bmpFiles <- filterM (doesFileExist . (folder ++)) . filter ((== ".bmp") . takeExtension) <$> listDirectory folder

  print <$> bmpFiles

  return ()



isolateArgs :: [String] -> Either String (FolderPath, Color, Order)
isolateArgs [f,c,o] =
  let folder = f -- No need to lowercase the folder path
      color = map toLower c
      order = map toLower o
  in case order of
    "asc" -> Right (folder, color, Asc)
    "desc" -> Right (folder, color, Desc)
    _ -> Left "Invalid order"
isolateArgs _ = Left "Invalid number of arguments"
