{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Indexer.Party (
  ContractTxOutParty (..),
  commitParties,
) where

import Data.ByteString (ByteString)
import Data.Int (Int16)
import qualified Data.Vector as V
import Hasql.TH (resultlessStatement)
import qualified Hasql.Transaction as H
import Language.Marlowe.Runtime.ChainSync.Api (TxId (..), TxIx (..), TxOutRef (..))
import Language.Marlowe.Runtime.Core.Api (ContractId (..))

data ContractTxOutParty = ContractTxOutParty
  { contractOut :: TxOutRef
  , contractId :: ContractId
  , rolesCurrency :: ByteString
  , partyAddress :: Maybe ByteString
  , partyRole :: Maybe ByteString
  }
  deriving (Eq, Show)

commitParties :: [ContractTxOutParty] -> H.Transaction ()
commitParties parties =
  H.statement params statement
  where
    statement =
      [resultlessStatement|
        WITH addressParties (address, txId, txIx, createTxId, createTxIx) AS
          ( SELECT * FROM UNNEST
              ( $1 :: bytea[]
              , $2 :: bytea[]
              , $3 :: smallint[]
              , $4 :: bytea[]
              , $5 :: smallint[]
              )
          )
        , roleParties (rolesCurrency, role, txId, txIx, createTxId, createTxIx) AS
          ( SELECT * FROM UNNEST
              ( $6 :: bytea[]
              , $7 :: bytea[]
              , $8 :: bytea[]
              , $9 :: smallint[]
              , $10 :: bytea[]
              , $11 :: smallint[]
              )
          )
        , addressInserts AS
          ( INSERT INTO marlowe.contractTxOutPartyAddress (address, txId, txIx, createTxId, createTxIx)
            SELECT * from addressParties
          )
        INSERT INTO marlowe.contractTxOutPartyRole (rolesCurrency, role, txId, txIx, createTxId, createTxIx)
        SELECT * from roleParties
      |]
    partyVector = V.fromList parties
    params =
      ( addresses
      , addressTxIds
      , addressTxIxs
      , addressCreateTxIds
      , addressCreateTxIxs
      , roleCurrencies
      , roles
      , roleTxIds
      , roleTxIxs
      , roleCreateTxIds
      , roleCreateTxIxs
      )
    ( addresses
      , addressTxIds
      , addressTxIxs
      , addressCreateTxIds
      , addressCreateTxIxs
      ) = V.unzip5 $ V.mapMaybe encodeAddress partyVector
    ( roleCurrencies
      , roles
      , roleTxIds
      , roleTxIxs
      , roleCreateTxIds
      , roleCreateTxIxs
      ) = V.unzip6 $ V.mapMaybe encodeRole partyVector

encodeAddress :: ContractTxOutParty -> Maybe (ByteString, ByteString, Int16, ByteString, Int16)
encodeAddress ContractTxOutParty{..} = case partyAddress of
  Just addr -> Just (addr, unTxId $ txId contractOut, fromIntegral $ unTxIx $ txIx contractOut, unTxId $ txId $ unContractId contractId, fromIntegral $ unTxIx $ txIx $ unContractId contractId)
  Nothing -> Nothing

encodeRole :: ContractTxOutParty -> Maybe (ByteString, ByteString, ByteString, Int16, ByteString, Int16)
encodeRole ContractTxOutParty{..} = case partyRole of
  Just role -> Just (rolesCurrency, role, unTxId $ txId contractOut, fromIntegral $ unTxIx $ txIx contractOut, unTxId $ txId $ unContractId contractId, fromIntegral $ unTxIx $ txIx $ unContractId contractId)
  Nothing -> Nothing
