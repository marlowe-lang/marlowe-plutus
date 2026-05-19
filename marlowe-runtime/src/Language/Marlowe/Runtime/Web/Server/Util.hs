module Language.Marlowe.Runtime.Web.Server.Util where

import Data.Function (on)
import qualified Data.List as List

import Cardano.Api (
  makeSignedTransaction,
 )
import Cardano.Ledger.Keys (KeyRole (Witness), WitVKey)

import Data.Set (Set)
import Servant.Pagination (RangeOrder (..))
import Control.Lens (over)
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Api as C

applyRangeToAscList :: (Eq f) => (a -> f) -> Maybe f -> Int -> Int -> RangeOrder -> [a] -> Maybe [a]
applyRangeToAscList getField startFrom limit offset order =
  fmap (List.take limit . List.drop offset)
    . case startFrom of
      Nothing -> Just
      Just f -> \as ->
        let as' = dropWhile ((/= f) . getField) as
         in if null as' then Nothing else Just as'
    . case order of
      RangeDesc -> reverse
      RangeAsc -> id
    . List.nubBy (on (==) getField)

type WitVKeys = Set (WitVKey 'Witness)

makeSignedTxWithWitnessKeys :: forall era. C.ConwayEraOnwards era -> C.TxBody era -> WitVKeys -> C.Tx era
makeSignedTxWithWitnessKeys eraWitness txBody wtKeys = case (eraWitness, makeSignedTransaction [] txBody) of
  (C.ConwayEraOnwardsConway, C.ShelleyTx era tx) ->
    C.ShelleyTx era $ over (Ledger.witsTxL . Ledger.addrTxWitsL) (const wtKeys) tx
  (C.ConwayEraOnwardsDijkstra, C.ShelleyTx era tx) ->
    C.ShelleyTx era $ over (Ledger.witsTxL . Ledger.addrTxWitsL) (const wtKeys) tx

