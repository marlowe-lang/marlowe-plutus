{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad.Except (runExceptT)
import Data.List (isSuffixOf)
import Marlowe.Plutus.Testing.Reference (processContract, referenceContractFiles)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getArgs)
import System.FilePath ((<.>), (</>), dropExtension)

-- Creates paths files for the contract files
main :: IO ()
main = do
  targets0 <- getArgs
  defaultTargets <- referenceContractFiles
  let targets
        | null targets0 = defaultTargets
        | otherwise = targets0
      findContracts target = do
        isDirectory <- doesDirectoryExist target
        if isDirectory
          then fmap ((target </>) . dropExtension) . filter (".contract" `isSuffixOf`) <$> listDirectory target
          else do
            isFile <- doesFileExist target
            pure
              [ dropExtension target
              | isFile
              , ".contract" `isSuffixOf` target
              ]
  prefixes <- concat <$> mapM findContracts targets
  sequence_
    [ runExceptT (processContract (prefix <.> "contract") (prefix <.> "paths"))
        >>= \case
          Right () -> putStrLn $ "Successfully processed " <> show prefix <> "."
          Left msg -> putStrLn $ "Failed processing " <> show prefix <> ": " <> msg <> "."
    | prefix <- prefixes
    ]
