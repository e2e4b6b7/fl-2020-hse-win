module Main where

import           PrologParser (parseText)
import           System.IO

parseFromFile :: FilePath -> IO ()
parseFromFile path = do
  input <- readFile path
  case parseText input of
    Left err -> print err
    Right r  -> writeFile (path ++ ".out") (show r)

main :: IO ()
main = parseFromFile "input.txt"
