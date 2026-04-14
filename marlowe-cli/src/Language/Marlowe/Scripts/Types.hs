module Language.Marlowe.Scripts.Types (
  module Marlowe.Plutus.Scripts.Types,
  marloweTxInputsFromInputs,
) where

import Marlowe.Plutus.Scripts.Types hiding (marloweTxInputsFromInputs)
import Marlowe.Plutus.Semantics.Types (Input)

marloweTxInputsFromInputs :: [Input] -> [MarloweTxInput]
marloweTxInputsFromInputs = error "TODO: marloweTxInputsFromInputs"