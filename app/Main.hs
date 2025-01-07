module Main where

import Control.Monad
import Data.Binary.Get
import Data.Bits
import Data.Char
import Data.Either
import Data.List as L
import Data.List.Split as S
import Data.Word
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import qualified Data.ByteString.Lazy as B

type FolderPath = String -- Path to the folder containing the files
type Color = String
data Order = Asc | Desc deriving (Show, Read)

usage :: String
usage = "Usage: ./sort-color <folder path> <color> <order>"

main :: IO ()
main = do
  args <- getArgs
  when (isLeft $ isolateArgs args ) $ mapM_ putStrLn ["Invalid arguments", usage]
  let Right (folder, color, order) = isolateArgs args

  files <- map (folder </>) <$> listDirectory folder
  bitmaps <- filterM (return . L.isSuffixOf ".bmp") files

  -- TODO Check if internal contents of bitmap are valid

  -- let sortedFiles = mapM (appendColorCount color) bitmaps

  return ()



-- | isolateArgs
-- | Given a list of strings, returns a tuple containing the folder path, color and order
-- | If the number of arguments is not 3, return an error message
-- | If the order is not "asc" or "desc", return an error message
-- | Otherwise, return the folder path, color and order
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

---------------------------------------------------------------
-- appendColorCount :: String -> FilePath -> (Int, FilePath) --
-- appendColorCount color file =                             --
--   let count = countColor color file                       --
--   in (count, file)                                        --
---------------------------------------------------------------

countColor :: Color -> FilePath -> IO Int
countColor color file = do

  -- Extract the pixel data from the bitmap as a list of RGB trios
  let colorData = ((map fromIntegral) <$>) <$> openBitmap file :: IO [[Int]]

  case color of
    "red" -> do
      colorData >>= return . sum . map (\[r,_,_] -> r)
    "green"-> do
      colorData >>= return . sum . map (\[_,g,_] -> g)
    "blue" -> do
      colorData >>= return . sum . map (\[_,_,b] -> b)
    _ -> return 0 -- TODO Deal with invalid color




openBitmap :: FilePath -> IO [[Word8]]
openBitmap file = do
  bmpData <- B.readFile file

  -- File data
  let fileType = B.take 2 bmpData
  guard $ fileType == B.pack [0x42, 0x4D] -- Check if the file is internally a bitmap

  let offset = runGet getWord32le $ B.drop 10 bmpData -- Offset to the pixel array
      pixelData = B.drop (fromIntegral offset) bmpData -- Extract the pixel data

  return $ map reverse $ S.chunksOf 3 $ B.unpack pixelData -- Split the pixel data into RGB color trios
