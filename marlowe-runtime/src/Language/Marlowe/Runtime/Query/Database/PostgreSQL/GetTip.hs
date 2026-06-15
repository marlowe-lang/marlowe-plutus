{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Query.Database.PostgreSQL.GetTip where

import Hasql.TH (maybeStatement)
import qualified Hasql.Transaction as T
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader (..), BlockHeaderHash (..), WithGenesis (..), ChainTip, chainTipFromChainPoint)

type GetTip m = m ChainTip

-- FIXME: We should introduce a new table which could be something like `chainTip`
-- with a few attributes like `slotNo`, `blockId`, `blockNo`. Those should be represented
-- through:
--
-- CREATE TYPE ledger_attr AS ENUM ('tip');
--
-- CREATE TABLE IF NOT EXISTS ledger_state
--   ( value bytea
--   , attr ledger_attr PRIMARY KEY
--   );
--
getTip :: GetTip T.Transaction
getTip =
  T.statement () $ do
    point <- decodePoint
        <$> [maybeStatement|
          SELECT
            block.slotNo :: bigint,
            block.id :: bytea,
            block.blockNo :: bigint
          FROM marlowe.block
          ORDER BY block.slotNo DESC
          LIMIT 1
        |]
    pure $ chainTipFromChainPoint point
  where
    decodePoint = \case
      Nothing -> Genesis
      Just (slot, hash, block) ->
        At $
          BlockHeader
            (fromIntegral slot)
            (BlockHeaderHash hash)
            (fromIntegral block)
