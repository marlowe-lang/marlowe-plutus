{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test where

import Control.Exception (bracket, SomeException, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import Test.HUnit (Assertion, assertBool, assertEqual, assertFailure, Test(TestCase, TestLabel, TestList))

import qualified Hasql as H
import qualified Hasql.Session as HS
import qualified Hasql.Transaction as HT
import qualified Hasql.Connection as HC

import Language.Marlowe.Runtime.ChainSync.Api 
  ( BlockHeader(..), BlockHeaderHash(..), BlockNo(..), SlotNo(..)
  , TxId(..), TxIx(..), TxOutRef(..), Address(..), ScriptHash(..)
  , ChainPoint(..), Assets(..), Tokens(..), TxOutAssets(..)
  , Lovelace(..), Quantity(..), PolicyId(..), TokenName(..), AssetId(..)
  )
import Language.Marlowe.Runtime.Core.Api (ContractId(..), MarloweVersion(..), SomeMarloweVersion(..))
import Language.Marlowe.Runtime.History.Api (UnspentContractOutput(..))
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.CommitRollback (commitRollback)
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.GetIntersectionPoints (getIntersectionPoints)
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.GetMarloweUTxO (getMarloweUTxO)
import Language.Marlowe.Runtime.Indexer.Types (MarloweBlock(..), MarloweTransaction(..))

defaultHost :: String
defaultHost = "localhost"

defaultPort :: String
defaultPort = "15432"

defaultDatabase :: String
defaultDatabase = "marlowe"

defaultUser :: String
defaultUser = "postgres"

defaultPassword :: String
defaultPassword = "postgres"

getConnectionString :: IO ByteString
getConnectionString = do
  host <- fromMaybe defaultHost <$> lookupEnv "MARLOWE_INDEXER_TEST_DB_HOST"
  port <- fromMaybe defaultPort <$> lookupEnv "MARLOWE_INDEXER_TEST_DB_PORT"
  db <- fromMaybe defaultDatabase <$> lookupEnv "MARLOWE_INDEXER_TEST_DB_NAME"
  user <- fromMaybe defaultUser <$> lookupEnv "MARLOWE_INDEXER_TEST_DB_USER"
  password <- fromMaybe defaultPassword <$> lookupEnv "MARLOWE_INDEXER_TEST_DB_PASSWORD"
  pure $ BS.pack $ "host=" ++ host ++ " port=" ++ port ++ " dbname=" ++ db ++ " user=" ++ user ++ " password=" ++ password

withConnection :: (HC.Connection -> IO a) -> IO a
withConnection action = do
  connStr <- getConnectionString
  result <- HC.acquire connStr
  case result of
    Left err -> error $ "Failed to acquire connection: " ++ show err
    Right releaseConn -> bracket releaseConn HC.release action

clearDatabase :: HC.Connection -> IO ()
clearDatabase conn = do
  let sql = BS.pack $ unlines
        [ "TRUNCATE TABLE marlowe.contractTxOutTag CASCADE"
        , "TRUNCATE TABLE marlowe.withdrawalTxIn CASCADE"
        , "TRUNCATE TABLE marlowe.invalidApplyTx CASCADE"
        , "TRUNCATE TABLE marlowe.payoutTxOut CASCADE"
        , "TRUNCATE TABLE marlowe.applyTx CASCADE"
        , "TRUNCATE TABLE marlowe.createTxOut CASCADE"
        , "TRUNCATE TABLE marlowe.contractTxOut CASCADE"
        , "TRUNCATE TABLE marlowe.txOutAsset CASCADE"
        , "TRUNCATE TABLE marlowe.txOut CASCADE"
        , "TRUNCATE TABLE marlowe.rollbackBlock CASCADE"
        , "TRUNCATE TABLE marlowe.block CASCADE"
        ]
  result <- HS.exec conn sql
  case result of
    Left err -> hPutStrLn stderr $ "Warning: Could not clear tables: " ++ show err
    Right _ -> pure ()

initializeDatabase :: HC.Connection -> IO ()
initializeDatabase conn = do
  result <- HS.exec conn setupSql
  case result of
    Left err -> hPutStrLn stderr $ "Warning during DB setup: " ++ show err
    Right _ -> pure ()
  where
  setupSql = BS.pack $ unlines
    [ "CREATE TABLE IF NOT EXISTS marlowe.block ("
    , "  id bytea PRIMARY KEY,"
    , "  slotNo bigint NOT NULL,"
    , "  blockNo bigint NOT NULL"
    , ");"
    , ""
    , "CREATE TABLE IF NOT EXISTS marlowe.rollbackBlock ("
    , "  fromBlock bytea PRIMARY KEY REFERENCES marlowe.block(id),"
    , "  toSlotNo bigint NOT NULL,"
    , "  toBlock bytea NOT NULL"
    , ");"
    , ""
    , "CREATE TABLE IF NOT EXISTS marlowe.txOut ("
    , "  txId bytea NOT NULL,"
    , "  txIx smallint NOT NULL,"
    , "  blockId bytea NOT NULL REFERENCES marlowe.block(id),"
    , "  address bytea NOT NULL,"
    , "  lovelace bigint NOT NULL,"
    , "  PRIMARY KEY (txId, txIx)"
    , ");"
    ]

runTest :: String -> IO Assertion -> Test
runTest name test = TestCase $ do
  putStrLn $ "  Running: " ++ name
  try test >>= \case
    Right () -> putStrLn $ "    PASSED: " ++ name
    Left (e :: SomeException) -> do
      hPutStrLn stderr $ "    FAILED: " ++ name
      hPutStrLn stderr $ "    Error: " ++ show e
      assertFailure $ name ++ ": " ++ show e

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
  initializeDatabase conn
  result <- HS.run (HT.run (HT.statement () (commitRollback Genesis))) conn
  case result of
    Left err -> assertFailure $ "Transaction failed: " ++ show err
    Right () -> pure ()

testCommitBlockAndQuery :: IO ()
testCommitBlockAndQuery = withConnection $ \conn -> do
  clearDatabase conn
  initializeDatabase conn
  
  let blockHeader = BlockHeader 
        { slotNo = SlotNo 100
        , headerHash = BlockHeaderHash (BS.pack "abc123")
        , blockNo = BlockNo 1
        }
  
  result <- HS.run (HT.run (HT.statement () (commitRollback (At blockHeader)))) conn
  case result of
    Left err -> assertFailure $ "Transaction failed: " ++ show err
    Right () -> pure ()

testGetIntersectionPointsEmpty :: IO ()
testGetIntersectionPointsEmpty = withConnection $ \conn -> do
  clearDatabase conn
  initializeDatabase conn
  result <- HS.run (HT.run (HT.statement () (getIntersectionPoints 10))) conn
  case result of
    Left err -> assertFailure $ "Query failed: " ++ show err
    Right points -> assertEqual "Should return empty list when no blocks" [] points

testGetMarloweUTxOEmpty :: IO ()
testGetMarloweUTxOEmpty = withConnection $ \conn -> do
  clearDatabase conn
  initializeDatabase conn
  
  let blockHeader = BlockHeader 
        { slotNo = SlotNo 0
        , headerHash = BlockHeaderHash (BS.pack "")
        , blockNo = BlockNo 0
        }
  
  result <- HS.run (HT.run (HT.statement () (getMarloweUTxO blockHeader))) conn
  case result of
    Left err -> assertFailure $ "Query failed: " ++ show err
    Right (contractOutputs, payoutOutputs) -> do
      assertEqual "Should have no unspent contract outputs" [] contractOutputs
      assertEqual "Should have no unspent payout outputs" [] payoutOutputs
