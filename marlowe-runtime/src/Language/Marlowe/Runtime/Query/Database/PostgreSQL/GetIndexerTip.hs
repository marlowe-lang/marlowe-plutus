{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Query.Database.PostgreSQL.GetIndexerTip where

import Control.Error (note)
import Data.Bifunctor (bimap)
import Data.Binary (get)
import Data.Binary.Get (runGetOrFail)
import Data.ByteString (ByteString, fromStrict)
import Data.Profunctor (rmap)
import Hasql.Statement (Statement)
import Hasql.TH (maybeStatement)
import Hasql.Transaction qualified as T
import Language.Marlowe.Runtime.ChainSync.Api (IndexerTip)
import Language.Marlowe.Runtime.Query.Database (GetIndexerTipError (MissingIndexerTip, InvalidIndexerTip))

type GetIndexerTip m = m (Either GetIndexerTipError IndexerTip)

getIndexerTip :: GetIndexerTip T.Transaction
getIndexerTip = do
  let
    s :: Statement () (Maybe ByteString)
    s =
      [maybeStatement|
          SELECT indexer_status.value :: bytea
          FROM marlowe.indexer_status
          WHERE indexer_status.attr = 'tip'
          LIMIT 1
      |]
  T.statement () $ flip rmap s \possibleTip -> do
    tipBytes <- note MissingIndexerTip possibleTip
    bimap (const $ InvalidIndexerTip tipBytes) (\(_, _, tip) -> tip) . runGetOrFail get . fromStrict $ tipBytes



