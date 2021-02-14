module Main where

import Data.List
import Codec.Picture (DynamicImage)
import qualified Codec.Picture as J
import System.Environment
import System.Exit
import System.FilePath
import qualified Transform as T
import qualified Data.Strings as S
import qualified Data.List as L

data OutputFormat = PNG | JPG deriving (Eq, Show, Read)

data Options = Options {
  transforms :: [String],
  inputPath :: FilePath,
  outputPath :: FilePath,
  outputFormat :: OutputFormat
} deriving (Eq, Show)

usage :: IO a
usage = do
  putStrLn "usage: swishi [option...] [transform...] <image path> <output path>"
  putStrLn "options:"
  putStrLn "  -l list transforms"
  putStrLn "  -f output format"
  putStrLn "  -h print help"
  exitFailure

main :: IO ()
main = do
  options <- parseOptions
  swishi options

-- FIXME: Ugh. Option parsing. Really?
parseOptions :: IO Options
parseOptions = do
  args <- getArgs
  args <- checkHelp args
  args <- checkListOpt args
  args <- checkLength args
  return $ extractOptions args
  where
    checkHelp args = if "-h" `elem` args then usage else return args
    checkListOpt args = if "-l" `elem` args then listTransforms else return args
    checkLength args = if length args < 2 then usage else return args

extractFormat :: [String] -> OutputFormat
extractFormat args = case L.elemIndex "-f" args of
  Just n -> read (args !! (n+1)) :: OutputFormat
  Nothing -> PNG -- TODO: Use the input image format

extractOptions :: [String] -> Options
extractOptions args = result where
  n = length args - 2
  transforms = take n args
  rest = drop n args
  inputPath = head rest
  outputPath = last rest
  result = Options {
    Main.transforms = transforms,
    inputPath = inputPath,
    outputPath = outputPath,
    outputFormat = extractFormat args
  }

listTransforms :: IO a
listTransforms = do
  mapM_ printTransform T.transforms
  exitSuccess
  where
    printTransform transform = putStrLn $ T.shortSummary transform

swishi :: Options -> IO ()
swishi options = do
  img <- readImageOrDie (inputPath options)
  mapM_ (applyOne img) selected
  where
    selected = selectTransforms (transforms options)
    applyOne img t = applyTransform options t img

readImageOrDie :: FilePath -> IO DynamicImage
readImageOrDie path = do
  img <- J.readImage path
  case img of
    Left err -> error err
    Right img -> return img

-- note: O(M*N)
selectTransforms :: [String] -> [T.Transform]
selectTransforms [] = []
selectTransforms (name:rest) = selected ++ selectTransforms rest
  where
    selected = filter (\t -> T.name t == name) T.transforms

applyTransform :: Options -> T.Transform -> DynamicImage -> IO ()
applyTransform opts transform img = do
  save destination result
  where
    result = T.apply transform img
    destination = outputPath opts
    save = saveFn opts

saveFn :: Options -> (FilePath -> DynamicImage -> IO ())
saveFn opts = case outputFormat opts of
  PNG -> J.savePngImage
  JPG -> J.saveJpgImage 9 -- quality=9
