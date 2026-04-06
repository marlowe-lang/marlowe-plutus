-- | Minimal stub
module Language.Marlowe.Runtime.Plutus.V2.Api where

import Language.Marlowe.Runtime.ChainSync.Api (Address(..))
import PlutusLedgerApi.V2 (toBuiltin)
import qualified PlutusLedgerApi.V2 as PV2

toPlutusAddress :: Address -> Maybe PV2.Address
toPlutusAddress (Address bs) = Just $ PV2.Address (PV2.PubKeyCredential $ PV2.PubKeyHash $ toBuiltin bs) Nothing

fromPlutusValue :: a -> b
fromPlutusValue = error "stub"

toAssetId :: a -> b
toAssetId = error "stub"

toPlutusCurrencySymbol :: a -> b
toPlutusCurrencySymbol = error "stub"

toPlutusTokenName :: a -> b
toPlutusTokenName = error "stub"
