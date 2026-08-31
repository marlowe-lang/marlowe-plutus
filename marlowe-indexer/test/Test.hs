{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test where

import Control.Exception (bracket, SomeException, try)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.Text as Text
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import Test.HUnit (assertEqual, Test(TestCase, TestLabel, TestList))

import qualified Hasql.Connection as HC
import qualified Hasql.Connection.Settings as Settings
import qualified Hasql.Session as HS
import qualified Hasql.Transaction.Sessions as HT

import Language.Marlowe.Runtime.ChainSync.Api 
  ( BlockHeader(..), BlockHeaderHash(..), BlockNo(..), SlotNo(..)
  , WithGenesis(..)
  )
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.CommitRollback (commitRollback)
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.GetIntersectionPoints (getIntersectionPoints, SecurityParameter(..))
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.GetMarloweUTxO (getMarloweUTxO)
import Language.Marlowe.Runtime.Indexer.MarloweBlock (MarloweUTxO(..))

defaultHost :: String
defaultHost = "127.0.0.1"

defaultPort :: String
defaultPort = "15432"

defaultDatabase :: String
defaultDatabase = "marlowe"

defaultUser :: String
defaultUser = "marlowe"

defaultPassword :: String
defaultPassword = ""

getConnectionSettings :: IO Settings.Settings
getConnectionSettings = do
  host <- fromMaybe defaultHost <$> lookupEnv "MARLOWE_INDEXER_TEST_DB_HOST"
  portStr <- fromMaybe defaultPort <$> lookupEnv "MARLOWE_INDEXER_TEST_DB_PORT"
  let port = fromIntegral (read portStr :: Int)
  db <- fromMaybe defaultDatabase <$> lookupEnv "MARLOWE_INDEXER_TEST_DB_NAME"
  user <- fromMaybe defaultUser <$> lookupEnv "MARLOWE_INDEXER_TEST_DB_USER"
  password <- fromMaybe defaultPassword <$> lookupEnv "MARLOWE_INDEXER_TEST_DB_PASSWORD"
  let settings = Settings.hostAndPort (Text.pack host) port
              <> Settings.user (Text.pack user)
              <> Settings.dbname (Text.pack db)
              <> Settings.password (Text.pack password)
  pure settings

withConnection :: (HC.Connection -> IO a) -> IO a
withConnection action = do
  settings <- getConnectionSettings
  result <- HC.acquire settings
  case result of
    Left err -> error $ "Failed to acquire connection: " ++ show err
    Right conn -> bracket (pure conn) HC.release action

clearDatabase :: HC.Connection -> IO ()
clearDatabase conn = do
  let sqls =
        [ "TRUNCATE TABLE marlowe.contractTxOutTag CASCADE"
        , "TRUNCATE TABLE marlowe.withdrawalTxIn CASCADE"
        , "TRUNCATE TABLE marlowe.invalidApplyTx CASCADE"
        , "TRUNCATE TABLE marlowe.payoutTxOut CASCADE"
        , "TRUNCATE TABLE marlowe.applyTx CASCADE"
        , "TRUNCATE TABLE marlowe.createTxOut CASCADE"
        , "TRUNCATE TABLE marlowe.contractTxOut CASCADE"
        , "TRUNCATE TABLE marlowe.contractTxOutPartyRole CASCADE"
        , "TRUNCATE TABLE marlowe.txOutAsset CASCADE"
        , "TRUNCATE TABLE marlowe.txOut CASCADE"
        , "TRUNCATE TABLE marlowe.rollbackBlock CASCADE"
        , "TRUNCATE TABLE marlowe.block CASCADE"
        ]
  sequence_ $ HC.use conn . HS.script . Text.pack <$> sqls

runTest :: String -> IO () -> Test
runTest name test = TestCase $ do
  putStrLn $ "  Running: " ++ name
  try test >>= \case
    Right () -> putStrLn $ "    PASSED: " ++ name
    Left (e :: SomeException) -> do
      hPutStrLn stderr $ "    FAILED: " ++ name
      hPutStrLn stderr $ "    Error: " ++ show e

allTests :: Test
allTests = TestList
  [ TestLabel "CommitRollback" commitRollbackTests
  , TestLabel "GetIntersectionPoints" getIntersectionPointsTests
  , TestLabel "GetMarloweUTxO" getMarloweUTxOTests
  ]

commitRollbackTests :: Test
commitRollbackTests = TestList
  [ runTest "commitGenesisBlock" testCommitGenesisBlock
  , runTest "commitBlockAndQuery" testCommitBlockAndQuery
  ]

getIntersectionPointsTests :: Test
getIntersectionPointsTests = TestList
  [ runTest "getIntersectionPointsEmpty" testGetIntersectionPointsEmpty
  ]

getMarloweUTxOTests :: Test
getMarloweUTxOTests = TestList
  [ runTest "getMarloweUTxOEmpty" testGetMarloweUTxOEmpty
  ]

testCommitGenesisBlock :: IO ()
testCommitGenesisBlock = withConnection $ \conn -> do
  clearDatabase conn
  let tx = HT.transaction HT.ReadCommitted HT.Write (commitRollback Genesis)
  result <- HC.use conn tx
  case result of
    Left err -> error $ "Transaction failed: " ++ show err
    Right _ -> pure ()

testCommitBlockAndQuery :: IO ()
testCommitBlockAndQuery = withConnection $ \conn -> do
  clearDatabase conn
  let blockHeader = BlockHeader 
        { slotNo = SlotNo 100
        , headerHash = BlockHeaderHash (BS.pack "abc123")
        , blockNo = BlockNo 1
        }
  let tx = HT.transaction HT.ReadCommitted HT.Write (commitRollback (At blockHeader))
  result <- HC.use conn tx
  case result of
    Left err -> error $ "Transaction failed: " ++ show err
    Right _ -> pure ()

testGetIntersectionPointsEmpty :: IO ()
testGetIntersectionPointsEmpty = withConnection $ \conn -> do
  clearDatabase conn
  let tx = HT.transaction HT.ReadCommitted HT.Write (getIntersectionPoints $ SecurityParameter 10)
  result <- HC.use conn tx
  case result of
    Left err -> error $ "Query failed: " ++ show err
    Right points -> assertEqual "Should return empty list when no blocks" [] points

testGetMarloweUTxOEmpty :: IO ()
testGetMarloweUTxOEmpty = withConnection $ \conn -> do
  clearDatabase conn
  let slotNo = SlotNo 0
  let tx = HT.transaction HT.ReadCommitted HT.Write (getMarloweUTxO slotNo)
  result <- HC.use conn tx
  case result of
    Left err -> error $ "Query failed: " ++ show err
    Right MarloweUTxO{..} -> do
      assertEqual "Should have no unspent contract outputs" [] (fst <$> Map.toList unspentContractOutputs)
      assertEqual "Should have no unspent payout outputs" [] (fst <$> Map.toList unspentPayoutOutputs)
