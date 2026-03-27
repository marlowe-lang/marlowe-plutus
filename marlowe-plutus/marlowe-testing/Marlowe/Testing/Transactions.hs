module Marlowe.Testing.Transactions (
  PayoutTransaction (..),
  SemanticsTransaction (..),

  amount,
  amountLens,
  input,
  inputContract,
  inputContractLens,
  inputLens,
  inputState,
  inputStateLens,
  marloweParams,
  marloweParamsPayout,
  output,
  outputLens,
  paramsLens',
  paramsLens,
  role,
  roleLens,
) where

import Control.Lens (Lens', lens)
import Language.Marlowe.Plutus.Semantics (MarloweParams, TransactionInput, TransactionOutput)
import Language.Marlowe.Plutus.Semantics.Types (Contract, State)
import PlutusLedgerApi.V1 (TokenName, Value)
import Marlowe.Testing.Contrib.PlutusTx.PlutusTransaction (PlutusTransaction, parameters)

data SemanticsTransaction = SemanticsTransaction
  { _params :: MarloweParams
  -- ^ The parameters.
  , _state :: State
  -- ^ The incoming state.
  , _contract :: Contract
  -- ^ The incoming contract.
  , _input :: TransactionInput
  -- ^ The transaction input.
  , _output :: TransactionOutput
  -- ^ The transaction output.
  }
  deriving (Show)

paramsLens :: Lens' SemanticsTransaction MarloweParams
paramsLens = lens _params $ \s x -> s{_params = x}

marloweParams :: Lens' (PlutusTransaction SemanticsTransaction) MarloweParams
marloweParams = parameters . paramsLens

inputStateLens :: Lens' SemanticsTransaction State
inputStateLens = lens _state $ \s x -> s{_state = x}

inputState :: Lens' (PlutusTransaction SemanticsTransaction) State
inputState = parameters . inputStateLens

inputContractLens :: Lens' SemanticsTransaction Contract
inputContractLens = lens _contract $ \s x -> s{_contract = x}

inputContract :: Lens' (PlutusTransaction SemanticsTransaction) Contract
inputContract = parameters . inputContractLens

inputLens :: Lens' SemanticsTransaction TransactionInput
inputLens = lens _input $ \s x -> s{_input = x}

input :: Lens' (PlutusTransaction SemanticsTransaction) TransactionInput
input = parameters . inputLens

outputLens :: Lens' SemanticsTransaction TransactionOutput
outputLens = lens _output $ \s x -> s{_output = x}

output :: Lens' (PlutusTransaction SemanticsTransaction) TransactionOutput
output = parameters . outputLens

-- | A Marlowe payout transaction.
data PayoutTransaction = PayoutTransaction
  { _params' :: MarloweParams
  -- ^ The parameters.
  , _role :: TokenName
  -- ^ The role name.
  , _amount :: Value
  -- ^ The value paid.
  }
  deriving (Show)

paramsLens' :: Lens' PayoutTransaction MarloweParams
paramsLens' = lens _params' $ \s x -> s{_params' = x}

marloweParamsPayout :: Lens' (PlutusTransaction PayoutTransaction) MarloweParams
marloweParamsPayout = parameters . paramsLens'

roleLens :: Lens' PayoutTransaction TokenName
roleLens = lens _role $ \s x -> s{_role = x}

role :: Lens' (PlutusTransaction PayoutTransaction) TokenName
role = parameters . roleLens

amountLens :: Lens' PayoutTransaction Value
amountLens = lens _amount $ \s x -> s{_amount = x}

amount :: Lens' (PlutusTransaction PayoutTransaction) Value
amount = parameters . amountLens

