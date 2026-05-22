module Main where

import Control.Monad (join)
import Options (mkOptions)
import Options.Applicative (execParser)

main :: IO ()
main = do
  options <- mkOptions
  join (execParser options)
