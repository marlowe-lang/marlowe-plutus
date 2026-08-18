{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Query.Database.PostgreSQL.GetMarloweTip where

import Hasql.TH (maybeStatement)
import qualified Hasql.Transaction as T
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader (..), BlockHeaderHash (..), MarloweTip(..), ChainTip(..))

getMarloweTip :: T.Transaction (Maybe MarloweTip)
getMarloweTip =
  T.statement () $
    decodePoint
      <$> [maybeStatement|
    SELECT
      block.slotNo :: bigint,
      block.id :: bytea,
      block.blockNo :: bigint
    FROM marlowe.block
    ORDER BY block.slotNo DESC
    LIMIT 1
  |]
  where
    decodePoint = fmap \(slot, hash, blockNo) -> MarloweTip
      . ChainTip
      . Just
      . BlockHeader (fromIntegral slot) (BlockHeaderHash hash)
      $ fromIntegral blockNo
