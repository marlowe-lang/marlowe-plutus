{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Cardano.Api.Kupo where
-- Ripped from kupo project. I isolated this for ease of development.
-- It can be merged into the core when tested.
--
-- FIXME: credit that in the README.

import qualified Cardano.Chain.UTxO as Ledger.Byron
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Plutus as Ledger
import qualified Cardano.Ledger.Api.Tx as Ledger
import qualified Cardano.Ledger.Shelley.Tx as Ledger.Shelley

import Cardano.Ledger.Allegra
    ( AllegraEra
    )
import Cardano.Ledger.Alonzo
    ( AlonzoEra
    )
import Cardano.Ledger.Babbage
    ( BabbageEra
    )
import Cardano.Ledger.Conway
    ( ConwayEra
    )
-- import Cardano.Ledger.Dijkstra
--     ( DijkstraEra
--     )
import Cardano.Ledger.Mary
    ( MaryEra
    )
import Cardano.Ledger.Shelley
    ( ShelleyEra
    )
import Data.Binary (Word16)
import Data.Set (Set)
import qualified Cardano.Ledger.TxIn as Ledger
import Control.Lens ((^.))
import qualified Data.Map as Map
import GHC.Stack (HasCallStack)
import Data.ByteString (ByteString)
import Data.ByteString.Short (toShort)
-- import qualified Cardano.Ledger.Conway.Scripts as Ledger
-- import qualified Cardano.Ledger.Dijkstra.Scripts as Ledger
import qualified Cardano.Api as C

type InputIndex = Word16

data KupoTransaction
    = TransactionByron
        !Ledger.Byron.Tx
        !Ledger.Byron.TxId
    | TransactionShelley
        !(Ledger.Shelley.Tx Ledger.TopTx ShelleyEra)
    | TransactionAllegra
        !(Ledger.Shelley.Tx Ledger.TopTx AllegraEra)
    | TransactionMary
        !(Ledger.Shelley.Tx Ledger.TopTx MaryEra)
    | TransactionAlonzo
        !(Ledger.Shelley.Tx Ledger.TopTx AlonzoEra)
    | TransactionBabbage
        !(Ledger.Shelley.Tx Ledger.TopTx BabbageEra)
    | TransactionConway
        !(Ledger.Shelley.Tx Ledger.TopTx ConwayEra)
--    | TransactionDijkstra
--        !(Ledger.Shelley.Tx Ledger.TopTx DijkstraEra)

spentInputs
    :: KupoTransaction
    -> Set Ledger.TxIn
spentInputs = \case
    TransactionByron _ _ -> mempty
    TransactionShelley tx ->
        tx ^. Ledger.bodyTxL . Ledger.inputsTxBodyL
    TransactionAllegra tx ->
        tx ^. Ledger.bodyTxL . Ledger.inputsTxBodyL
    TransactionMary tx ->
        tx ^. Ledger.bodyTxL . Ledger.inputsTxBodyL
    TransactionAlonzo tx ->
        case tx ^. Ledger.isValidTxL of
            Ledger.IsValid True ->
                tx ^. Ledger.bodyTxL . Ledger.inputsTxBodyL
            Ledger.IsValid False ->
                tx ^. Ledger.bodyTxL . Ledger.collateralInputsTxBodyL
    TransactionBabbage tx ->
        case tx ^. Ledger.isValidTxL of
            Ledger.IsValid True ->
                tx ^. Ledger.bodyTxL . Ledger.inputsTxBodyL
            Ledger.IsValid False ->
                tx ^. Ledger.bodyTxL . Ledger.collateralInputsTxBodyL
    TransactionConway tx ->
        case tx ^. Ledger.isValidTxL of
            Ledger.IsValid True ->
                tx ^. Ledger.bodyTxL . Ledger.inputsTxBodyL
            Ledger.IsValid False ->
                tx ^. Ledger.bodyTxL . Ledger.collateralInputsTxBodyL
--    TransactionDijkstra tx ->
--        case tx ^. Ledger.isValidTxL of
--            Ledger.IsValid True ->
--                tx ^. Ledger.bodyTxL . Ledger.inputsTxBodyL
--            Ledger.IsValid False ->
--                tx ^. Ledger.bodyTxL . Ledger.collateralInputsTxBodyL

data Redeemers
    = RedeemersAlonzo (Ledger.Redeemers AlonzoEra)
    | RedeemersBabbage (Ledger.Redeemers BabbageEra)
    | RedeemersConway (Ledger.Redeemers ConwayEra)
--     | RedeemersDijkstra (Ledger.Redeemers DijkstraEra)

type CurrentEra = ConwayEra

unsafeBinaryDataFromBytes
    :: HasCallStack
    => ByteString
    -> Ledger.BinaryData CurrentEra
unsafeBinaryDataFromBytes =
    either error id . Ledger.makeBinaryData . toShort
{-# INLINABLE unsafeBinaryDataFromBytes #-}

fromAlonzoData
    :: HasCallStack
    => Ledger.Data AlonzoEra
    -> Ledger.BinaryData CurrentEra
fromAlonzoData =
    unsafeBinaryDataFromBytes . Ledger.originalBytes
{-# INLINEABLE fromAlonzoData #-}

fromBabbageData
    :: Ledger.Data BabbageEra
    -> Ledger.BinaryData CurrentEra
fromBabbageData =
      Ledger.dataToBinaryData
    . Ledger.upgradeData
{-# INLINEABLE fromBabbageData #-}

fromConwayData
    :: Ledger.Data ConwayEra
    -> Ledger.BinaryData CurrentEra
fromConwayData =
      Ledger.dataToBinaryData
    . Ledger.upgradeData
{-# INLINEABLE fromConwayData #-}

-- fromDijkstraData
--     :: Ledger.Data CurrentEra
--     -> Ledger.BinaryData CurrentEra
-- fromDijkstraData =
--     Ledger.dataToBinaryData
-- {-# INLINEABLE fromDijkstraData #-}

lookupSpendRedeemer
    :: InputIndex
    -> Redeemers
    -> Maybe (Ledger.BinaryData CurrentEra)
lookupSpendRedeemer ix = \case
    RedeemersAlonzo (Ledger.Redeemers redeemers) ->
        let purpose = Ledger.AlonzoSpending (Ledger.AsIx (fromIntegral ix))
         in fromAlonzoData . fst <$> Map.lookup purpose redeemers
    RedeemersBabbage (Ledger.Redeemers redeemers) ->
        let purpose = Ledger.AlonzoSpending (Ledger.AsIx (fromIntegral ix))
         in fromBabbageData . fst <$> Map.lookup purpose redeemers
    RedeemersConway (Ledger.Redeemers redeemers) ->
        let purpose = Ledger.ConwaySpending (Ledger.AsIx (fromIntegral ix))
         in fromConwayData . fst <$> Map.lookup purpose redeemers
    -- RedeemersDijkstra (Ledger.Redeemers redeemers) ->
    --     let purpose = Ledger.DijkstraSpending (Ledger.AsIx (fromIntegral ix))
    --      in fromDijkstraData . fst <$> Map.lookup purpose redeemers


spendRedeemer
    :: KupoTransaction
    -> InputIndex
    -> Maybe (Ledger.BinaryData CurrentEra)
spendRedeemer txInEra ix =
    lookupSpendRedeemer ix =<< case txInEra of
        TransactionByron{} ->
            Nothing
        TransactionShelley{} ->
            Nothing
        TransactionAllegra{} ->
            Nothing
        TransactionMary{} ->
            Nothing
        TransactionAlonzo tx ->
            Just (RedeemersAlonzo (tx ^. Ledger.witsTxL . Ledger.rdmrsTxWitsL))
        TransactionBabbage tx ->
            Just (RedeemersBabbage (tx ^. Ledger.witsTxL . Ledger.rdmrsTxWitsL))
        TransactionConway tx ->
            Just (RedeemersConway (tx ^. Ledger.witsTxL . Ledger.rdmrsTxWitsL))
        -- TransactionDijkstra tx ->
        --     Just (RedeemersDijkstra (tx ^. Ledger.witsTxL . Ledger.rdmrsTxWitsL))

txFromCardanoTx :: forall era. C.IsCardanoEra era => C.Tx era -> KupoTransaction
txFromCardanoTx tx = case (C.cardanoEra @era, tx) of
  (C.ShelleyEra, C.ShelleyTx _ tx') -> TransactionShelley tx'
  (C.AllegraEra, C.ShelleyTx _ tx') -> TransactionAllegra tx'
  (C.MaryEra, C.ShelleyTx _ tx') -> TransactionMary tx'
  (C.AlonzoEra, C.ShelleyTx _ tx') -> TransactionAlonzo tx'
  (C.BabbageEra, C.ShelleyTx _ tx') -> TransactionBabbage tx'
  (C.ConwayEra, C.ShelleyTx _ tx') -> TransactionConway tx'
  (C.DijkstraEra, C.ShelleyTx _ _tx') -> error "Kupo.txFromCardanoTx: Dijkstra era not supported yet."
    -- TransactionDijkstra tx'
  (C.ByronEra, _) -> error "Kupo.txFromCardanoTx: Impossible. Byron tx was removed from cardano-api."


