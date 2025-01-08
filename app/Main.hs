module Main where

import Control.Monad
import Data.Binary.Get
import Data.Char
import Data.Either
import Data.Function
import Data.Word
import System.Directory
import System.Environment
import System.FilePath
import Text.Read
import qualified Data.ByteString.Lazy as B
import qualified Data.List as L
import qualified Data.List.Split as S

type FolderPath = String -- Path to the folder containing the bitmaps
data Color = Red | Green | Blue deriving (Read)
data Order = Asc | Desc deriving (Read)

type Args = (FolderPath, Color, Order) -- Command line arguments
type RankedFiles = [(Int, FilePath)] -- List of ranked files with their rank and path

main :: IO ()
main = do
  args <- getArgs

  when (isLeft $ isolateArgs args ) $ putStrLn $ fromLeft "" (isolateArgs args)
  let Right (folder, color, order) = isolateArgs args

  files <- getFilePaths folder
  bitmaps <- filterBitmapFiles files

  colorCountedBitmaps <- mapM (prependColorCount color) bitmaps

  let sortedFiles = zip [1..] $ sortByOrder order colorCountedBitmaps

  putStrLn "Ranking:" >> mapM_ (putStrLn . show) sortedFiles

  renameBitmaps sortedFiles folder

  putStrLn "Files renamed successfully"

isolateArgs :: [String] -> Either String (String, Color, Order)
isolateArgs [folder, color, order] =
  case (readMaybe (toTitleCase color), readMaybe (toTitleCase order)) of
    (Just c, Just o) -> Right (folder, c, o)
    _ -> Left "Invalid color or order. Valid colors (case-insensitive): Red, Green, Blue. Valid orders: Asc, Desc."
isolateArgs _ = Left "Invalid number of arguments, usage: ./sort-color <folder path> <color> <order>"

toTitleCase :: String -> String
toTitleCase (x:xs) = toUpper x : map toLower xs
toTitleCase []     = []

getFilePaths :: FolderPath -> IO [FilePath]
getFilePaths folder = map (folder </>) <$> listDirectory folder

filterBitmapFiles :: [FilePath] -> IO [FilePath]
filterBitmapFiles = filterM (return . L.isSuffixOf ".bmp")

-- Reimplementation using radix sort
sortByOrder :: Order -> RankedFiles -> [FilePath]
sortByOrder order xs =
  let sortingOrder = case order of
        Asc -> id
        Desc -> reverse
      sortedList = sortByOrder' (map fst xs) (map snd xs)
  in sortingOrder sortedList

sortByOrder' :: [Int] -> [FilePath] -> [FilePath]
sortByOrder' [] _ = []
sortByOrder' _ [] = []
sortByOrder' keys filePaths =
  let buckets = foldr (\(key, file) acc ->
                         let index = key `mod` 10
                         in take index acc ++ [file : acc !! index] ++ drop (index + 1) acc
                      )
                      (replicate 10 [])
                      (zip keys filePaths)
      sortedBuckets = map reverse buckets
      flattened = concat sortedBuckets
  in flattened

prependColorCount :: Color -> FilePath -> IO (Int, FilePath)
prependColorCount color file = do
  count <- countColor color file
  return (count, file)

countColor :: Color -> FilePath -> IO Int
countColor color file = do

  -- Extract the pixel data from the bitmap as a list of RGB trios
  let colorData = do
        bitmapData <- openBitmap file
        return $ map (map fromIntegral) bitmapData -- Convert the Word8 values to Int

  colorDataLength <- length <$> colorData

  -- HACK This deals with non trios of RGB values by returning 0
  case color of
    Red -> colorData >>= return . sum . map (\list -> case list of { [r,_,_] -> r; (r:_) -> r; _ -> 0 })
                  >>= \total -> return (total `div` colorDataLength)
    Green -> colorData >>= return . sum . map (\list -> case list of { [_,g,_] -> g; [_,g] -> g; [_] -> 0; [] -> 0 })
                  >>= \total -> return (total `div` colorDataLength)
    Blue -> colorData >>= return . sum . map (\list -> case list of { [_,_,b] -> b; [_,b] -> b; [_] -> 0; [] -> 0 })
                  >>= \total -> return (total `div` colorDataLength)

openBitmap :: FilePath -> IO [[Word8]]
openBitmap file = do
  bmpData <- B.readFile file

  guard $ (B.take 2 bmpData) == B.pack [0x42, 0x4D] -- Check if the file is internally a bitmap

  let offset = runGet getWord32le $ B.drop 10 bmpData -- Offset to the pixel array
      pixelData = B.drop (fromIntegral offset) bmpData -- Extract the pixel data

  -- Colors are store in BGR format in the bitmap, so we need to reverse the order of the bytes
  return $ map reverse $ S.chunksOf 3 $ B.unpack pixelData

renameBitmaps :: RankedFiles -> FilePath -> IO ()
renameBitmaps rankedFiles folder =
  forM_ rankedFiles $ \(rank, file) -> do
    let filename = takeFileName file
    let newFile = folder </> show rank <> "-" <> filename
    renameFile file newFile
