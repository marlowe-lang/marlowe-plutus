-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Function to generate all valid transactions for contracts in JSON files.
module Marlowe.Testing.Reference (
  -- * Types
  FixtureName (..),
  ReferencePath (..),
  ReferenceTransaction (..),

  -- * Analysis
  processContract,

  -- * Testing
  arbitraryReferenceTransaction,
  referenceContractFiles,
  referenceTransactions,
  readReferenceContracts,
  readReferencePaths,
  readReferencePathsFor,
) where

import Control.Monad (forM)
import Control.Monad.Except (ExceptT (..), throwError)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile)
import Data.Bifunctor (first)
import Data.List (isSuffixOf)
import GHC.Generics (Generic)
import Language.Marlowe.Plutus.Semantics (TransactionInput, TransactionOutput (..), computeTransaction)
import Language.Marlowe.Plutus.Semantics.Types (Contract, State (..), ada, mkRoleByteString)
import Language.Marlowe.Plutus.FindInputs (getAllInputs)
import PlutusLedgerApi.V2 (POSIXTime)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeBaseName)

import Language.Marlowe.Analysis.FSSemantics (SlotLength (..))
import qualified Language.Marlowe.Plutus.AssocMap as AM (empty, singleton)
import Paths_marlowe_plutus (getDataDir)
import Test.QuickCheck (Gen, elements)
import Marlowe.Testing.Semantics.Golden (GoldenTransaction)

newtype FixtureName = FixtureName String
  deriving (Eq, Show)

dataFilesWithSuffix :: String -> IO [FilePath]
dataFilesWithSuffix suffix = do
  dataDir <- getDataDir
  go dataDir
 where
  go folder = do
    entries <- listDirectory folder
    fmap concat . forM entries $ \entry -> do
      let path = folder </> entry
      isDirectory <- doesDirectoryExist path
      if isDirectory
        then go path
        else pure [path | suffix `isSuffixOf` path]

referenceContractFiles :: IO [FilePath]
referenceContractFiles = dataFilesWithSuffix ".contract"

readReferenceContracts :: IO [(FilePath, Contract)]
readReferenceContracts = readReferenceContracts' =<< referenceContractFiles

readReferenceContracts' :: [FilePath] -> IO [(FilePath, Contract)]
readReferenceContracts' contractFiles =
  forM contractFiles $
    \contractFile ->
      eitherDecodeFileStrict contractFile
        >>= \case
          Right contract -> pure (contractFile, contract)
          Left msg -> error $ "Failed parsing " <> contractFile <> ": " <> msg <> "."

readReferencePaths :: IO [ReferencePath]
readReferencePaths = do
  pathFiles <- dataFilesWithSuffix ".paths"
  -- let pathFiles = ["deposit.paths"]
  readReferencePaths' pathFiles

readReferencePathsFor :: FixtureName -> IO [ReferencePath]
readReferencePathsFor (FixtureName fixtureName) = do
  pathFiles <- filter ((== fixtureName) . takeBaseName) <$> dataFilesWithSuffix ".paths"
  case pathFiles of
    [] -> error $ "Failed locating fixture paths for " <> show fixtureName <> "."
    [pathFile] -> readReferencePaths' [pathFile]
    _ -> error $ "Ambiguous fixture paths for " <> show fixtureName <> ": " <> show pathFiles <> "."

readReferencePaths' :: [FilePath] -> IO [ReferencePath]
readReferencePaths' pathFiles =
  fmap concat
    . forM pathFiles
    $ \pathFile ->
      eitherDecodeFileStrict pathFile
        >>= \case
          Right paths -> pure $ filter (not . null . transactions) paths
          Left msg -> error $ "Failed parsing " <> pathFile <> ": " <> msg <> "."

arbitraryReferenceTransaction :: [ReferencePath] -> Gen GoldenTransaction
arbitraryReferenceTransaction = elements . referenceTransactions

referenceTransactions :: [ReferencePath] -> [GoldenTransaction]
referenceTransactions = concatMap referencePathTransactions
 where
  referencePathTransactions ReferencePath{..}
    | length transactions > 1 =
        [ (txOutState prior, txOutContract prior, input, output)
        | (ReferenceTransaction _ prior, ReferenceTransaction{..}) <- zip (init transactions) (tail transactions)
        ]
    | otherwise =
        [ let ReferenceTransaction{..} = head transactions
           in (state, contract, input, output)
        ]

data ReferencePath = ReferencePath
  { contract :: Contract
  , state :: State
  , transactions :: [ReferenceTransaction]
  }
  deriving (Generic, Show)

instance FromJSON ReferencePath

instance ToJSON ReferencePath

data ReferenceTransaction = ReferenceTransaction
  { input :: TransactionInput
  , output :: TransactionOutput
  }
  deriving (Generic, Show)

instance FromJSON ReferenceTransaction

instance ToJSON ReferenceTransaction

processContract
  :: FilePath
  -> FilePath
  -> ExceptT String IO ()
processContract contractFile pathsFile =
  do
    let slotLength = SlotLength 1_000
    contract <- ExceptT $ first show <$> eitherDecodeFileStrict contractFile
    traces <- ExceptT $ first show <$> getAllInputs slotLength contract Nothing
    paths <- runTransactions contract `mapM` traces
    lift $ encodeFile pathsFile paths

runTransactions
  :: Contract
  -> (POSIXTime, [TransactionInput])
  -> ExceptT String IO ReferencePath
runTransactions contract (startTime, inputs) =
  do
    let state = makeState startTime
    transactions <- runTransaction contract state inputs
    pure ReferencePath{..}

runTransaction
  :: Contract
  -> State
  -> [TransactionInput]
  -> ExceptT String IO [ReferenceTransaction]
runTransaction _ _ [] = pure []
runTransaction contract state (input : inputs) =
  case computeTransaction input state contract of
    Error err -> throwError $ show err
    output@TransactionOutput{..} -> (ReferenceTransaction{..} :) <$> runTransaction txOutContract txOutState inputs

makeState
  :: POSIXTime
  -> State
makeState minTime =
  let accounts = AM.singleton (mkRoleByteString "", ada) 30_000_000 -- Note that 30 ada exceeds min-UTxO for current protocol parameters.
      choices = AM.empty
      boundValues = AM.empty
   in State{..}
