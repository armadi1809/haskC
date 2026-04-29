module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

data DriverOptions = DriverOptions
  { srcFile :: Maybe FilePath,
    upToLex :: Bool,
    upToParse :: Bool,
    upToCodeGen :: Bool
  }

defaultOptions :: DriverOptions
defaultOptions = DriverOptions {srcFile = Nothing, upToParse = False, upToLex = False, upToCodeGen = False}

parseArgs :: DriverOptions -> [String] -> Either String DriverOptions
parseArgs opts [] = Right opts
parseArgs opts (x : xs) = case x of
  "--lex" -> parseArgs (opts {upToLex = True}) xs
  "--parse" -> parseArgs (opts {upToParse = True}) xs
  "--codegen" -> parseArgs (opts {upToCodeGen = True}) xs
  _ -> if (take 1 x) == "-" then Left "Unknown flag" else parseArgs (opts {srcFile = Just x}) xs

main :: IO ()
main = do
  args <- getArgs
  let compOptions = parseArgs defaultOptions args

  case compOptions of
    Left err -> do
      putStrLn err
      exitFailure
    Right ops -> case (srcFile ops) of
      Nothing -> putStrLn "Usage: haskC <file>"
      Just _ -> exitSuccess