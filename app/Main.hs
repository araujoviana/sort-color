module Main where

import Control.Monad
import Data.Binary.Get
import Data.Char
import Data.Either
import Data.Word
import System.Directory
import System.Environment
import System.FilePath
import qualified Data.ByteString.Lazy as B
import qualified Data.List as L
import qualified Data.List.Split as S


type FolderPath = String -- Path to the folder containing the bitmaps
type Color = String
data Order = Asc | Desc deriving (Read)

type Args = (FolderPath, Color, Order) -- Command line arguments

usage :: String
usage = "Usage: ./sort-color <folder path> <color> <order>"

main :: IO ()
main = do
  args <- getArgs

  when (isLeft $ isolateArgs args ) $ putStrLn usage
  let Right (folder, color, order) = isolateArgs args

  files <- getFilePaths folder
  bitmaps <- getBitmaps files

  colorCountedBitmaps <- mapM (prependColorCount color) bitmaps

  let rankedFiles = zip [1..] $ sortByOrder order colorCountedBitmaps

  putStrLn "Ranking:" >> mapM_ (putStrLn . show) rankedFiles

  renameBitmaps rankedFiles folder

  putStrLn "Files renamed successfully"


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

getFilePaths :: FolderPath -> IO [FilePath]
getFilePaths folder = map (folder </>) <$> listDirectory folder

getBitmaps :: [FilePath] -> IO [FilePath]
getBitmaps = filterM (return . L.isSuffixOf ".bmp")

sortByOrder :: Order -> [(Int, FilePath)] -> [FilePath]
sortByOrder Asc xs = map snd $ L.sortBy (\(a,_) (b,_) -> compare a b) xs
sortByOrder Desc xs = map snd $ L.sortBy (\(a,_) (b,_) -> compare b a) xs

prependColorCount :: String -> FilePath -> IO (Int, FilePath)
prependColorCount color file = do
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


renameBitmaps :: [(Int, FilePath)] -> FilePath -> IO ()
renameBitmaps rankedFiles folder =
  forM_ rankedFiles $ \(rank, file) -> do
    let filename = takeFileName file
    let newFile = folder </> show rank <> "-" <> filename
    renameFile file newFile
