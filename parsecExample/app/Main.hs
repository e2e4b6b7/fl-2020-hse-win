module Main where

import           PrologParser       (parseText)
import           System.Environment (getArgs)
import           System.IO          ()

parseFromFile :: FilePath -> IO ()
parseFromFile path = do
  input <- readFile path
  case parseText input of
    Left err -> print err
    Right r  -> writeFile (path ++ ".out") (show r)

main :: IO ()
main = do
  args <- getArgs
  parseFromFile $ head args
