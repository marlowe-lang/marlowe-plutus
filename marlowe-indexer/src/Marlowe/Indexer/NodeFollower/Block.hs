{-# OPTIONS_GHC -Wno-deprecations #-}

module Marlowe.Indexer.NodeFollower.Block (
  txCount,
  withCardanoEraTxs
) where

import Cardano.Api (
  BlockInMode (..),
 )
import Cardano.Chain.UTxO qualified as Byron
import Ouroboros.Consensus.Byron.Ledger qualified as Byron
import qualified Cardano.Api as C
import qualified Cardano.Chain.Block as LB

txCount :: C.BlockInMode -> Int
txCount (BlockInMode _era block) = case block of
  C.ByronBlock byronBlock -> case Byron.byronBlockRaw byronBlock of
    LB.ABOBBlock (LB.ABlock _ body _) -> do
      let
        txPayload = Byron.aUnTxPayload (LB.bodyTxPayload body)
      length txPayload
    _ -> error "Unexpected non-ABOB Byron Block"
  _ -> length $ C.getBlockTxs block


withCardanoEraTxs :: forall a era. C.Block era -> (C.IsCardanoEra era => [C.Tx era] -> a) -> a
withCardanoEraTxs block f = case block of
  C.ByronBlock _ -> f []
  C.ShelleyBlock sbe _shelleyBlockRaw -> C.shelleyBasedEraConstraints sbe $
    f $ C.getBlockTxs block
