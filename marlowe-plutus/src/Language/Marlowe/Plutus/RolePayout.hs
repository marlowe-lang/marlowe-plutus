{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

-- | Marlowe role-payout validator.
module Language.Marlowe.Plutus.RolePayout (
  -- * Validation
  mkRolePayoutValidator,
) where

import PlutusLedgerApi.V2 (
  ScriptContext (scriptContextTxInfo),
 )
import PlutusLedgerApi.V2.Contexts (valueSpent)

import Language.Marlowe.Plutus.Semantics.Types as Semantics (
  CurrencySymbol,
  TokenName,
 )
import PlutusTx.Prelude (Bool)
import qualified PlutusLedgerApi.V1.Value as Val

-- | The Marlowe payout validator.
mkRolePayoutValidator
  :: (CurrencySymbol, TokenName)
  -- ^ The datum is the currency symbol and role name for the payout.
  -> ()
  -- ^ No redeemer is required.
  -> ScriptContext
  -- ^ The script context.
  -> Bool
  -- ^ Whether the transaction validated.
mkRolePayoutValidator (currency, role) _ ctx =
  -- The role token for the correct currency must be present.
  -- [Marlowe-Cardano Specification: "17. Payment authorized".]
  Val.singleton currency role 1 `Val.leq` valueSpent (scriptContextTxInfo ctx)

