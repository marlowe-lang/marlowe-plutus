module Marlowe.Plutus.Foldings (
  extractNonMerkleizedContractRoles,
  foldMapContract,
  foldMapNonMerkleizedContract,
) where

-- The only open import brings in the Marlowe types.
import Marlowe.Plutus.Semantics.Types

import qualified Data.Set as Set
import Data.Set (Set)
import qualified PlutusLedgerApi.V1.Value as Val
import qualified PlutusTx.Prelude as P


-- FIXME: Add monadic version so we can short-circuit, error out, etc.
foldMapContract
  :: (Monoid m)
  => (P.BuiltinByteString -> Maybe Contract)
  -> (Contract -> m)
  -> (Case Contract -> m)
  -> (Observation -> m)
  -> (Value Observation -> m)
  -> Contract
  -> m
foldMapContract funmerk fcont fcase fobs fvalue contract =
  fcont contract <> case contract of
    Close -> mempty
    Pay _ _ _ value cont -> fvalue' value <> go cont
    If obs cont1 cont2 -> fobs' obs <> go cont1 <> go cont2
    When cases _ cont -> foldMap fcase' cases <> go cont
    Let _ value cont -> fvalue value <> go cont
    Assert obs cont -> fobs' obs <> go cont
  where
    go = foldMapContract funmerk fcont fcase fobs fvalue
    fcase' cs =
      fcase cs <> case cs of
        Case _ cont -> go cont
        MerkleizedCase _ chash -> maybe mempty go (funmerk chash)
    fobs' obs =
      fobs obs <> case obs of
        AndObs a b -> fobs' a <> fobs' b
        OrObs a b -> fobs' a <> fobs' b
        NotObs a -> fobs' a
        ValueGE a b -> fvalue' a <> fvalue' b
        ValueGT a b -> fvalue' a <> fvalue' b
        ValueLT a b -> fvalue' a <> fvalue' b
        ValueLE a b -> fvalue' a <> fvalue' b
        ValueEQ a b -> fvalue' a <> fvalue' b
        _ -> mempty
    fvalue' v =
      fvalue v <> case v of
        NegValue val -> fvalue' val
        AddValue a b -> fvalue' a <> fvalue' b
        SubValue a b -> fvalue' a <> fvalue' b
        MulValue a b -> fvalue' a <> fvalue' b
        DivValue a b -> fvalue' a <> fvalue' b
        Cond obs a b -> fobs' obs <> fvalue' a <> fvalue' b
        _ -> mempty

-- FIXME: This default is risky and can be misinterpretted.
foldMapNonMerkleizedContract
  :: (Monoid m)
  => (Contract -> m)
  -> (Case Contract -> m)
  -> (Observation -> m)
  -> (Value Observation -> m)
  -> Contract
  -> m
foldMapNonMerkleizedContract = foldMapContract (const Nothing)

extractNonMerkleizedContractRoles :: Contract -> Set Val.TokenName
extractNonMerkleizedContractRoles = foldMapNonMerkleizedContract extract extractCase (const mempty) (const mempty)
  where
    extract (Pay from payee _ _ _) = fromParty from <> fromPayee payee
    extract _ = mempty

    extractCase (Case (Deposit acc party _ _) _) = fromParty acc <> fromParty party
    extractCase (Case (Choice (ChoiceId _ party) _) _) = fromParty party
    extractCase _ = mempty

    fromParty (Role name) = Set.singleton name
    fromParty _ = mempty

    fromPayee (Party party) = fromParty party
    fromPayee (Account party) = fromParty party
