module Marlowe.Testing.Transactions (
  PayoutTransaction (..),
  SemanticsTransaction (..),

  -- * Debugging
  payoutTransactionToJSON,
  semanticsTransactionToJSON,

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
import PlutusLedgerApi.V1 (TokenName (TokenName), Value, fromBuiltin)
import Marlowe.Testing.Contrib.PlutusTx.PlutusTransaction (PlutusTransaction, parameters)
import qualified Data.Aeson as A
import Data.ByteString.Base16.Aeson (EncodeBase16(..))
import Marlowe.Testing.Contrib.PlutusTx.Debugging.Aeson (valueToJSON)

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

semanticsTransactionToJSON :: SemanticsTransaction -> A.Value
semanticsTransactionToJSON SemanticsTransaction{..} = A.object
  [ "params" A..= A.toJSON _params
  , "state" A..= A.toJSON _state
  , "contract" A..= A.toJSON _contract
  , "input" A..= A.toJSON _input
  , "output" A..= A.toJSON _output
  ]

instance A.ToJSON SemanticsTransaction where
  toJSON = semanticsTransactionToJSON

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

payoutTransactionToJSON :: PayoutTransaction -> A.Value
payoutTransactionToJSON PayoutTransaction{..} = A.object
  [ "params" A..= A.toJSON _params'
  , "role" A..= do
    let
      TokenName (fromBuiltin -> roleName) = _role
    A.toJSON $ EncodeBase16 roleName
  , "amount" A..= valueToJSON _amount
  ]

instance A.ToJSON PayoutTransaction where
  toJSON = payoutTransactionToJSON
