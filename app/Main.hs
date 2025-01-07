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

-- TODO
--------------------------------------------------------------
-- appendColorCount :: color -> FilePath -> (Int, FilePath) --
-- appendColorCount color file =                            --
--   let count = countColor color file                      --
--   in (count, file)                                       --
--                                                          --
--                                                          --
-- countColor :: Color -> FilePath -> Int                   --
-- countColor color file =                                  --
--  undefined                                               --
--------------------------------------------------------------

openBitmap :: FilePath -> IO [[Word8]]
openBitmap file = do
  bmpData <- B.readFile file

  -- File data
  let fileType = B.take 2 bmpData
  guard $ fileType == B.pack [0x42, 0x4D] -- Check if the file is internally a bitmap

  let offset = runGet getWord32le $ B.drop 10 bmpData -- Offset to the pixel array
      pixelData = B.drop (fromIntegral offset) bmpData -- Extract the pixel data

  return $ map reverse $ S.chunksOf 3 $ B.unpack pixelData -- Split the pixel data into RGB color trios
