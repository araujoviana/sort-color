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
  when (isLeft $ isolateArgs args ) $ mapM_ putStrLn ["Invalid arguments", "Usage: ./sort-color <folder path> <color> <order>"]
  let Right (folder, color, order) = isolateArgs args

  files <- map (folder </>) <$> listDirectory folder
  bitmaps <- filterM (return . L.isSuffixOf ".bmp") files

  -- TODO Check if internal contents of bitmap are valid

  sortedFiles <- mapM (appendColorCount color) bitmaps :: IO [(Int, FilePath)]

  let rankedFiles = case order of
        Asc -> L.sortBy (\(a,_) (b,_) -> compare a b) sortedFiles
        Desc -> L.sortBy (\(a,_) (b,_) -> compare b a) sortedFiles
      rankedFilesWithRank = zip [1..] $ map snd rankedFiles

  putStrLn "Ranking:" >> mapM_ (putStrLn . show) rankedFilesWithRank

  forM_ rankedFilesWithRank $ \(rank, file) -> do
    let filename = takeFileName file
    let newFile = folder </> show rank <> "-" <> filename
    renameFile file newFile

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


appendColorCount :: String -> FilePath -> IO (Int, FilePath)
appendColorCount color file = do
  count <- countColor color file
  return (count, file)


countColor :: Color -> FilePath -> IO Int
countColor color file = do
  -- Extract the pixel data from the bitmap as a list of RGB trios
  let colorData = ((map fromIntegral) <$>) <$> openBitmap file :: IO [[Int]]

  colorDataLength <- length <$> colorData

  -- HACK This deals with non trios of RGB values by returning 0
  case color of
    "red" -> colorData >>= return . sum . map (\list -> case list of { [r,_,_] -> r; (r:_) -> r; _ -> 0 })
                  >>= \total -> return (total `div` colorDataLength)
    "green" -> colorData >>= return . sum . map (\list -> case list of { [_,g,_] -> g; [_,g] -> g; [_] -> 0; [] -> 0 })
                  >>= \total -> return (total `div` colorDataLength)
    "blue" -> colorData >>= return . sum . map (\list -> case list of { [_,_,b] -> b; [_,b] -> b; [_] -> 0; [] -> 0 })
                  >>= \total -> return (total `div` colorDataLength)
    _ -> error "Invalid color"

openBitmap :: FilePath -> IO [[Word8]]
openBitmap file = do
  bmpData <- B.readFile file

  guard $ (B.take 2 bmpData) == B.pack [0x42, 0x4D] -- Check if the file is internally a bitmap

  let offset = runGet getWord32le $ B.drop 10 bmpData -- Offset to the pixel array
      pixelData = B.drop (fromIntegral offset) bmpData -- Extract the pixel data

  -- Colors are store in BGR format in the bitmap, so we need to reverse the order of the bytes
  return $ map reverse $ S.chunksOf 3 $ B.unpack pixelData


