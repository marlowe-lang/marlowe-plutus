module Main where

import Test.HUnit
import Test

main :: IO ()
main = do
  putStrLn "Marlowe Indexer Database Test Suite"
  putStrLn "=================================="
  putStrLn ""
  runTestTT allTests
  return ()
