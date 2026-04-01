{-# LANGUAGE OverloadedStrings #-}

module Marlowe.Testing.ProtocolParams
  ( EvaluationContexts(..)
  , loadMainnetEvaluationContexts
  ) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Control.Monad.Trans.Writer (runWriterT)
import Control.Monad.Trans.Except (runExcept)

import qualified PlutusLedgerApi.V2 as PV2

import Paths_marlowe_plutus (getDataFileName)
import GHC.Int (Int64)

data ProtocolParams = ProtocolParams Int CostModelsRaw

-- | The cost models for PlutusV2 and PlutusV3, as raw lists of integers.
data CostModelsRaw = CostModelsRaw [Int64] [Int64]

instance Aeson.FromJSON ProtocolParams where
  parseJSON = Aeson.withObject "ProtocolParams" $ \o ->
    ProtocolParams
      <$> o Aeson..: "protocol_major_ver"
      <*> o Aeson..: "cost_models_raw"

instance Aeson.FromJSON CostModelsRaw where
  parseJSON = Aeson.withObject "CostModelsRaw" $ \o ->
    CostModelsRaw
      <$> o Aeson..: "PlutusV2"
      <*> o Aeson..: "PlutusV3"

data PlutusVersion2or3 = Version2 | Version3
  deriving (Eq, Ord, Show)

data EvaluationContexts = EvaluationContexts
  { majorProtocolVersion :: PV2.MajorProtocolVersion
  , v2 :: PV2.EvaluationContext
  , v3 :: PV2.EvaluationContext
  }

loadMainnetEvaluationContexts
  :: IO (Either String EvaluationContexts)
loadMainnetEvaluationContexts = do
  file <- getDataFileName "marlowe-testing/data/mainnet-protocol-params.json"
  bytes <- LBS.readFile file
  pure $ do
    ProtocolParams major (CostModelsRaw v2 v3) <-
      first show $ Aeson.eitherDecode bytes

    let mpv = PV2.MajorProtocolVersion major
    v2ec <-
      first show
        . fmap fst
        . runExcept
        . runWriterT
        $ PV2.mkEvaluationContext v2
    v3ec <-
      first show
        . fmap fst
        . runExcept
        . runWriterT
        $ PV2.mkEvaluationContext v3
    pure $ EvaluationContexts mpv v2ec v3ec
