module Main where

import Abstraction
import IntervalAbstraction
import Language
import Memory
import SignAbstraction
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  if not (null args) && (head args == "--help" || head args == "-h")
    then do
      putStrLn "Usage: program [options] [file]"
      putStrLn "Options:"
      putStrLn "  --help, -h      Show this help message"
      putStrLn "  [file]          Input file containing the program to analyze (optional)"
    else
      analyzeProgram args

analyzeProgram :: [String] -> IO ()
analyzeProgram args = do
  input <- getProgram args
  let cmd = read input :: Command
  putStrLn $ "Parsed command: " ++ show cmd
  putStrLn $ "Pretty Print:\n\n" ++ pretty cmd ++ "\n"
  let signAnalysis = analyze cmd :: Memory SignAbstraction
  putStrLn $ "Sign Analysis: " ++ show signAnalysis
  let intervalAnalysis = analyze cmd :: Memory IntervalAbstraction
  putStrLn $ "Interval Analysis: " ++ show intervalAnalysis

getProgram :: [String] -> IO String
getProgram args = do
  if not (null args)
    then do
      let fileName = head args
      contents <- readFile fileName
      return $ concat (lines contents)
    else do
      putStrLn "Write program to analyze:"
      getLine
