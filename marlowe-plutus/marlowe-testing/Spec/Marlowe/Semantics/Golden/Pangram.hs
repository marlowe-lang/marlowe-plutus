-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Reference golden output for the Pangram contract.
module Spec.Marlowe.Semantics.Golden.Pangram (
  -- * Contracts
  contract,

  -- * Test cases
  invalids,
  valids,
) where

import Language.Marlowe.Plutus.Semantics (
  Payment (Payment),
  TransactionInput (..),
  TransactionOutput (..),
  TransactionWarning (..),
 )
import Language.Marlowe.Plutus.Semantics.Types (
  Action (Choice, Deposit, Notify),
  Bound (Bound),
  Case (Case),
  ChoiceId (ChoiceId),
  Contract (..),
  Input (NormalInput),
  InputContent (IChoice, IDeposit, INotify),
  Observation (AndObs, ChoseSomething, FalseObs, NotObs, OrObs, ValueEQ, ValueGE, ValueGT, ValueLE, ValueLT),
  Party,
  Payee (Account, Party),
  State (State, accounts, boundValues, choices, minTime),
  Token (Token),
  Value (
    AddValue,
    AvailableMoney,
    ChoiceValue,
    Cond,
    Constant,
    DivValue,
    MulValue,
    NegValue,
    SubValue,
    TimeIntervalEnd,
    TimeIntervalStart,
    UseValue
  ),
  ValueId (ValueId),
  ada,
  mkChoiceIdUtf8,
  mkRoleUtf8,
  mkTokenNameUtf8,
  mkValueIdUtf8,
  unsafeMkCurrencySymbolHex,
  unsafeMkPartyAddressBech32,
 )

import PlutusLedgerApi.V2 (POSIXTime (..))

import qualified Language.Marlowe.Plutus.AssocMap as AM (Map, unsafeFromList)

party1 :: Party
party1 =
  unsafeMkPartyAddressBech32
    "addr_test1vrssw4edcts00kk6lp7p5n64666m23tpprqaarmdwkaq69gfvqnpz"

party2 :: Party
party2 =
  unsafeMkPartyAddressBech32
    "addr_test1qp2l7afky3eqfkrht5f3qgy7x2yek5dejcnpnuqlwywz9twr7cz4mu6gh005gdck67p7y9d8s8zsfgjkcdy75mrjh6jqp8jwfw"

partyCy :: Party
partyCy = mkRoleUtf8 "Cy"

partyNoe :: Party
partyNoe = mkRoleUtf8 "Noe"

partySten :: Party
partySten = mkRoleUtf8 "Sten"

token1 :: Token
token1 = ada

token2 :: Token
token2 =
  Token
    (unsafeMkCurrencySymbolHex "13e78e78c233e131b0cbe4424225d338b7c5ac65e16df0a3e6c9d8f8")
    (mkTokenNameUtf8 "PIN")

token3 :: Token
token3 =
  Token
    (unsafeMkCurrencySymbolHex "1b9af43b0eaafc42dfaefbbf4e71437af45454c7292a6b6606363741")
    (mkTokenNameUtf8 "TALE")

choiceBeByParty1 :: ChoiceId
choiceBeByParty1 = mkChoiceIdUtf8 "be" party1

choiceBeByPartyCy :: ChoiceId
choiceBeByPartyCy = mkChoiceIdUtf8 "be" partyCy

choiceDryByPartyCy :: ChoiceId
choiceDryByPartyCy = mkChoiceIdUtf8 "dry" partyCy

choice4 :: ChoiceId
choice4 = mkChoiceIdUtf8 "grab" partyNoe

deposit1Id :: ValueId
deposit1Id = mkValueIdUtf8 "deposit1"

deposit2Id :: ValueId
deposit2Id = mkValueIdUtf8 "deposit2"

deposit3Id :: ValueId
deposit3Id = mkValueIdUtf8 "deposit3"

deposit4Id :: ValueId
deposit4Id = mkValueIdUtf8 "deposit4"

deposit5Id :: ValueId
deposit5Id = mkValueIdUtf8 "deposit5"

choose1Id :: ValueId
choose1Id = mkValueIdUtf8 "choose1"

choose2Id :: ValueId
choose2Id = mkValueIdUtf8 "choose2"

choose3Id :: ValueId
choose3Id = mkValueIdUtf8 "choose3"

xId :: ValueId
xId = mkValueIdUtf8 "x"

labId :: ValueId
labId = mkValueIdUtf8 "lab"

notify1Id :: ValueId
notify1Id = mkValueIdUtf8 "notify1"

notify2Id :: ValueId
notify2Id = mkValueIdUtf8 "notify2"

-- | The Pangram contract.
contract :: Contract
contract =
  let start = 1
      delta = 5
      timeout i = POSIXTime $ start + delta * i
      value1 = ChoiceValue choiceBeByParty1
      value2 = ChoiceValue choiceBeByPartyCy
      value3 = ChoiceValue choiceDryByPartyCy
      value4 = ChoiceValue choice4
      value5 = UseValue xId
      value6 = UseValue (ValueId "id")
      value7 = UseValue labId
      value8 = AvailableMoney party1 token1
      value9 = AvailableMoney party2 token2
      value10 = AvailableMoney partyCy token3
      value11 = AvailableMoney partyNoe token1
      value12 = AvailableMoney partySten token1
      value13 = Cond observation3 (value1 `AddValue` value5) (value8 `SubValue` TimeIntervalStart)
      value14 = value7 `MulValue` (value10 `DivValue` Constant 2)
      value15 = NegValue value9
      value16 = Cond observation5 (TimeIntervalEnd `DivValue` value7) (value6 `MulValue` value5)
      value17 = Cond observation4 value3 value7
      observation1 = ValueGE value1 (Constant 4) `OrObs` FalseObs
      observation2 = ValueGT value2 (Constant 6) `AndObs` NotObs (ChoseSomething choiceBeByPartyCy)
      observation3 = ValueLE value3 (Constant 9) `OrObs` ChoseSomething choiceDryByPartyCy
      observation4 = ValueLT value4 (Constant 1) `AndObs` observation2
      observation5 = ValueEQ value1 value2 `OrObs` NotObs observation1
      deposit1 = Case (Deposit party1 party2 token1 value5) . Let deposit1Id value5
      deposit2 = Case (Deposit party1 party1 token1 value6) . Let deposit2Id value6
      deposit3 = Case (Deposit partyCy partyCy token2 value7) . Let deposit3Id value7
      deposit4 = Case (Deposit partyNoe partyCy token1 value1) . Let deposit4Id value1
      deposit5 = Case (Deposit partyCy partySten token1 value2) . Let deposit5Id value2
      choose1 = Case (Choice choiceBeByParty1 [Bound 1 4, Bound 3 6]) . Let choose1Id value1
      choose2 = Case (Choice choiceBeByParty1 [Bound 5 8]) . Let choose2Id value2
      choose3 = Case (Choice choiceBeByPartyCy [Bound 1 8]) . Let choose3Id value3
      choose4 = Case (Choice choiceDryByPartyCy []) . Let (mkValueIdUtf8 "choose4") value4
      notify1 = Case (Notify observation1) . Let notify1Id (Cond observation1 (Constant 0) (Constant 1))
      notify2 = Case (Notify observation2) . Let notify2Id (Cond observation2 (Constant 0) (Constant 1))
      pay1 = Pay party1 (Party partyCy) token2 value13
      pay2 = Pay party2 (Party party1) token2 value14
      pay3 = Pay partyCy (Account party1) token2 value15
      pay4 = Pay partyNoe (Account partyNoe) token2 value16
      pay5 = Pay partySten (Party party2) token2 value17
      if1 c = If observation5 (let1 c) (let2 c)
      let1 = Let xId value11
      let2 = Let labId (value10 `AddValue` Constant 10 `AddValue` value12)
      assert1 = Assert (observation3 `OrObs` observation2)
      when i as c = When (fmap ($ c) as) (timeout i) Close -- TODO: Ideally, this should timeout to `c` instead of to `Close`, but the combinations are too many.
      both i a1 a2 c =
        When
          [ a1 $ When [a2 c] (timeout $ i + 1) c
          , a2 $ When [a1 c] (timeout $ i + 1) c
          ]
          (timeout i)
          c
   in When [] (timeout 1) $
        both 2 deposit1 deposit2 $
          both 4 notify1 choose2 $
            when 6 [deposit3, deposit4] $
              when 7 [choose1, choose2] $
                let1 $
                  when 8 [deposit5] $
                    let2 $
                      pay1 $
                        when 9 [choose3] $
                          pay2 $
                            when 10 [choose1] $
                              assert1 $
                                let1 $
                                  when 11 [choose4] $
                                    pay3 $
                                      if1 $
                                        pay4 $
                                          let2 $
                                            when 12 [notify2] $
                                              pay5
                                                Close

-- | A wrapper to assist parsing of test cases.
newtype Map k v = Map {unMap :: [(k, v)]}

-- | A function to assist parsing of test cases.
toAM :: Map k v -> AM.Map k v
toAM = AM.unsafeFromList . unMap

-- | A list of test cases and results that should succeed, generated from `Language.Marlowe.FindInputs.getAllInputs`.
valids :: [(POSIXTime, [TransactionInput], TransactionOutput)]
valids =
  [
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 31}, POSIXTime{getPOSIXTime = 31}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = []}
              , boundValues = toAM $ Map{unMap = []}
              , minTime = POSIXTime{getPOSIXTime = 31}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 36}, POSIXTime{getPOSIXTime = 36}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [TransactionNonPositiveDeposit partyCy partyCy token2 0]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = []}
              , boundValues = toAM $ Map{unMap = [(deposit3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 36}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [TransactionNonPositiveDeposit partyCy partyCy token2 0]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 1)]}
              , boundValues = toAM $ Map{unMap = [(deposit3Id, 0), (choose1Id, 1), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(choiceBeByParty1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit3Id, 0), (choose1Id, 5), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(choiceBeByParty1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $ Map{unMap = [(deposit3Id, 0), (choose1Id, 5), (xId, 0), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing choose1Id 5 5
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $ Map{unMap = [(deposit3Id, 0), (choose1Id, 5), (xId, 0), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [TransactionNonPositiveDeposit partyCy partyCy token2 0]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues = toAM $ Map{unMap = [(deposit3Id, 0), (choose2Id, 0), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues = toAM $ Map{unMap = [(deposit3Id, 0), (choose2Id, 0), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 7), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $ Map{unMap = [(deposit3Id, 0), (choose2Id, 0), (xId, 0), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit3Id, 0), (choose2Id, 0), (xId, 0), (deposit5Id, 0), (labId, 10), (choose3Id, 0), (choose1Id, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 36}, POSIXTime{getPOSIXTime = 36}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings = [TransactionNonPositiveDeposit partyCy partyNoe ada 0]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = []}
              , boundValues = toAM $ Map{unMap = [(deposit4Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 36}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings = [TransactionNonPositiveDeposit partyCy partyNoe ada 0]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit4Id, 0), (choose1Id, 5), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partyCy partyNoe ada 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit4Id, 0), (choose1Id, 5), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partyCy partyNoe ada 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $ Map{unMap = [(deposit4Id, 0), (choose1Id, 5), (xId, 0), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partyCy partyNoe ada 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing choose1Id 5 5
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $ Map{unMap = [(deposit4Id, 0), (choose1Id, 5), (xId, 0), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings = [TransactionNonPositiveDeposit partyCy partyNoe ada 0]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues = toAM $ Map{unMap = [(deposit4Id, 0), (choose2Id, 0), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partyCy partyNoe ada 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues = toAM $ Map{unMap = [(deposit4Id, 0), (choose2Id, 0), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partyCy partyNoe ada 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 7), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $ Map{unMap = [(deposit4Id, 0), (choose2Id, 0), (xId, 0), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partyCy partyNoe ada 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit4Id, 0), (choose2Id, 0), (xId, 0), (deposit5Id, 0), (labId, 10), (choose3Id, 0), (choose1Id, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 31}, POSIXTime{getPOSIXTime = 31}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(choose2Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 31}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 36}, POSIXTime{getPOSIXTime = 36}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [TransactionNonPositiveDeposit partyCy partyCy token2 0]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(choose2Id, 0), (deposit3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 36}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [TransactionNonPositiveDeposit partyCy partyCy token2 0]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 1)]}
              , boundValues = toAM $ Map{unMap = [(choose2Id, 0), (deposit3Id, 0), (choose1Id, 1), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                1
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 1)]}
              , boundValues =
                  toAM $ Map{unMap = [(choose2Id, 0), (deposit3Id, 0), (choose1Id, 1), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map{unMap = [(choose2Id, 0), (deposit3Id, 0), (choose1Id, 5), (xId, 0), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing choose1Id 5 1
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 1), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map{unMap = [(choose2Id, 0), (deposit3Id, 0), (choose1Id, 1), (xId, 0), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues = toAM $ Map{unMap = [(choose2Id, 0), (deposit3Id, 0), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues = toAM $ Map{unMap = [(choose2Id, 0), (deposit3Id, 0), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 7), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $ Map{unMap = [(choose2Id, 0), (deposit3Id, 0), (xId, 0), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map{unMap = [(choose2Id, 0), (deposit3Id, 0), (xId, 0), (deposit5Id, 0), (labId, 10), (choose3Id, 0), (choose1Id, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 36}, POSIXTime{getPOSIXTime = 36}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments = [Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(choose2Id, 0), (deposit4Id, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 36}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments = [Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(choose2Id, 0), (deposit4Id, 5), (choose1Id, 5), (xId, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                10
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues =
                  toAM $ Map{unMap = [(choose2Id, 0), (deposit4Id, 5), (choose1Id, 5), (xId, 5), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                6
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 1), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map{unMap = [(choose2Id, 0), (deposit4Id, 5), (choose1Id, 1), (xId, 5), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                10
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing choose1Id 5 1
            , TransactionShadowing xId 5 5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 1), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map{unMap = [(choose2Id, 0), (deposit4Id, 5), (choose1Id, 1), (xId, 5), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings = [TransactionShadowing choose2Id 0 0]
        , txOutPayments = [Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues = toAM $ Map{unMap = [(choose2Id, 0), (deposit4Id, 5), (xId, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                12
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues = toAM $ Map{unMap = [(choose2Id, 0), (deposit4Id, 5), (xId, 5), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                12
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 7), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $ Map{unMap = [(choose2Id, 0), (deposit4Id, 5), (xId, 5), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                12
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing xId 5 5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map{unMap = [(choose2Id, 0), (deposit4Id, 5), (xId, 5), (deposit5Id, 0), (labId, 10), (choose3Id, 0), (choose1Id, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 31}, POSIXTime{getPOSIXTime = 31}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(choose2Id, 0), (notify1Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 31}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 36}, POSIXTime{getPOSIXTime = 36}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [TransactionNonPositiveDeposit partyCy partyCy token2 0]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(choose2Id, 0), (notify1Id, 0), (deposit3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 36}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [TransactionNonPositiveDeposit partyCy partyCy token2 0]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 1)]}
              , boundValues = toAM $ Map{unMap = [(choose2Id, 0), (notify1Id, 0), (deposit3Id, 0), (choose1Id, 1), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues =
                  toAM $
                    Map{unMap = [(choose2Id, 0), (notify1Id, 0), (deposit3Id, 0), (choose1Id, 5), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                1
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 1), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit3Id, 0)
                          , (choose1Id, 1)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing choose1Id 5 5
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit3Id, 0)
                          , (choose1Id, 5)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues = toAM $ Map{unMap = [(choose2Id, 0), (notify1Id, 0), (deposit3Id, 0), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues =
                  toAM $ Map{unMap = [(choose2Id, 0), (notify1Id, 0), (deposit3Id, 0), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 7), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map{unMap = [(choose2Id, 0), (notify1Id, 0), (deposit3Id, 0), (xId, 0), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit3Id, 0)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          , (choose1Id, 5)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 36}, POSIXTime{getPOSIXTime = 36}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments = [Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(choose2Id, 0), (notify1Id, 0), (deposit4Id, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 36}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments = [Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 1)]}
              , boundValues = toAM $ Map{unMap = [(choose2Id, 0), (notify1Id, 0), (deposit4Id, 5), (choose1Id, 1), (xId, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                10
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues =
                  toAM $
                    Map{unMap = [(choose2Id, 0), (notify1Id, 0), (deposit4Id, 5), (choose1Id, 5), (xId, 5), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                6
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 1), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit4Id, 5)
                          , (choose1Id, 1)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                10
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing choose1Id 5 5
            , TransactionShadowing xId 5 5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit4Id, 5)
                          , (choose1Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings = [TransactionShadowing choose2Id 0 0]
        , txOutPayments = [Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues = toAM $ Map{unMap = [(choose2Id, 0), (notify1Id, 0), (deposit4Id, 5), (xId, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                12
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues =
                  toAM $ Map{unMap = [(choose2Id, 0), (notify1Id, 0), (deposit4Id, 5), (xId, 5), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                12
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 7), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map{unMap = [(choose2Id, 0), (notify1Id, 0), (deposit4Id, 5), (xId, 5), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                12
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing xId 5 5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit4Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          , (choose1Id, 5)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 31}, POSIXTime{getPOSIXTime = 31}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = []}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 31}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 36}, POSIXTime{getPOSIXTime = 36}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = []}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (deposit3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 36}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (deposit3Id, 0), (choose1Id, 5), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                1
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 1)]}
              , boundValues =
                  toAM $ Map{unMap = [(deposit1Id, 0), (deposit3Id, 0), (choose1Id, 1), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit1Id, 0), (deposit3Id, 0), (choose1Id, 5), (xId, 0), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing choose1Id 5 5
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit1Id, 0), (deposit3Id, 0), (choose1Id, 5), (xId, 0), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (deposit3Id, 0), (choose2Id, 0), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues =
                  toAM $ Map{unMap = [(deposit1Id, 0), (deposit3Id, 0), (choose2Id, 0), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 7), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit1Id, 0), (deposit3Id, 0), (choose2Id, 0), (xId, 0), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit3Id, 0)
                          , (choose2Id, 0)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          , (choose1Id, 5)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 36}, POSIXTime{getPOSIXTime = 36}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = []}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (deposit4Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 36}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (deposit4Id, 0), (choose1Id, 5), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues =
                  toAM $ Map{unMap = [(deposit1Id, 0), (deposit4Id, 0), (choose1Id, 5), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit1Id, 0), (deposit4Id, 0), (choose1Id, 5), (xId, 0), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing choose1Id 5 5
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit1Id, 0), (deposit4Id, 0), (choose1Id, 5), (xId, 0), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (deposit4Id, 0), (choose2Id, 0), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues =
                  toAM $ Map{unMap = [(deposit1Id, 0), (deposit4Id, 0), (choose2Id, 0), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 7), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit1Id, 0), (deposit4Id, 0), (choose2Id, 0), (xId, 0), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 1), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit4Id, 0)
                          , (choose2Id, 0)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          , (choose1Id, 1)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 31}, POSIXTime{getPOSIXTime = 31}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (choose2Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 31}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 36}, POSIXTime{getPOSIXTime = 36}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (choose2Id, 0), (deposit3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 36}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (choose2Id, 0), (deposit3Id, 0), (choose1Id, 5), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit1Id, 0), (choose2Id, 0), (deposit3Id, 0), (choose1Id, 5), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                1
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 1), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (deposit3Id, 0)
                          , (choose1Id, 1)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                1
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing choose1Id 1 5
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (deposit3Id, 0)
                          , (choose1Id, 5)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (choose2Id, 0), (deposit3Id, 0), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues =
                  toAM $ Map{unMap = [(deposit1Id, 0), (choose2Id, 0), (deposit3Id, 0), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 7), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit1Id, 0), (choose2Id, 0), (deposit3Id, 0), (xId, 0), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (deposit3Id, 0)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          , (choose1Id, 5)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 36}, POSIXTime{getPOSIXTime = 36}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            ]
        , txOutPayments = [Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (choose2Id, 0), (deposit4Id, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 36}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            ]
        , txOutPayments = [Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (choose2Id, 0), (deposit4Id, 5), (choose1Id, 5), (xId, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                6
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 1)]}
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit1Id, 0), (choose2Id, 0), (deposit4Id, 5), (choose1Id, 1), (xId, 5), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                10
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (deposit4Id, 5)
                          , (choose1Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                10
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing choose1Id 5 5
            , TransactionShadowing xId 5 5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (deposit4Id, 5)
                          , (choose1Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            ]
        , txOutPayments = [Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (choose2Id, 0), (deposit4Id, 5), (xId, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                12
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues =
                  toAM $ Map{unMap = [(deposit1Id, 0), (choose2Id, 0), (deposit4Id, 5), (xId, 5), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                12
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 7), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit1Id, 0), (choose2Id, 0), (deposit4Id, 5), (xId, 5), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                12
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing xId 5 5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (deposit4Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          , (choose1Id, 5)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 31}, POSIXTime{getPOSIXTime = 31}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (choose2Id, 0), (notify1Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 31}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 36}, POSIXTime{getPOSIXTime = 36}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (choose2Id, 0), (notify1Id, 0), (deposit3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 36}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues =
                  toAM $ Map{unMap = [(deposit1Id, 0), (choose2Id, 0), (notify1Id, 0), (deposit3Id, 0), (choose1Id, 5), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit3Id, 0)
                          , (choose1Id, 5)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit3Id, 0)
                          , (choose1Id, 5)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing choose1Id 5 1
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 1), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit3Id, 0)
                          , (choose1Id, 1)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (choose2Id, 0), (notify1Id, 0), (deposit3Id, 0), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit1Id, 0), (choose2Id, 0), (notify1Id, 0), (deposit3Id, 0), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 7), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit3Id, 0)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit3Id, 0)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          , (choose1Id, 5)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 36}, POSIXTime{getPOSIXTime = 36}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            ]
        , txOutPayments = [Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (choose2Id, 0), (notify1Id, 0), (deposit4Id, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 36}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            ]
        , txOutPayments = [Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues =
                  toAM $ Map{unMap = [(deposit1Id, 0), (choose2Id, 0), (notify1Id, 0), (deposit4Id, 5), (choose1Id, 5), (xId, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                10
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit4Id, 5)
                          , (choose1Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                10
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit4Id, 5)
                          , (choose1Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                10
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing choose1Id 5 5
            , TransactionShadowing xId 5 5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit4Id, 5)
                          , (choose1Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            ]
        , txOutPayments = [Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (choose2Id, 0), (notify1Id, 0), (deposit4Id, 5), (xId, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                12
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit1Id, 0), (choose2Id, 0), (notify1Id, 0), (deposit4Id, 5), (xId, 5), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                12
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 7), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit4Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                12
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing xId 5 5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 1), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit4Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          , (choose1Id, 1)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 31}, POSIXTime{getPOSIXTime = 31}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = []}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (deposit2Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 31}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 36}, POSIXTime{getPOSIXTime = 36}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = []}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (deposit2Id, 0), (deposit3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 36}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (deposit2Id, 0), (deposit3Id, 0), (choose1Id, 5), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit1Id, 0), (deposit2Id, 0), (deposit3Id, 0), (choose1Id, 5), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit2Id, 0)
                          , (deposit3Id, 0)
                          , (choose1Id, 5)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                1
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing choose1Id 1 5
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit2Id, 0)
                          , (deposit3Id, 0)
                          , (choose1Id, 5)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (deposit2Id, 0), (deposit3Id, 0), (choose2Id, 0), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit1Id, 0), (deposit2Id, 0), (deposit3Id, 0), (choose2Id, 0), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 7), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit2Id, 0)
                          , (deposit3Id, 0)
                          , (choose2Id, 0)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit2Id, 0)
                          , (deposit3Id, 0)
                          , (choose2Id, 0)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          , (choose1Id, 5)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 36}, POSIXTime{getPOSIXTime = 36}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = []}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (deposit2Id, 0), (deposit4Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 36}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 1)]}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (deposit2Id, 0), (deposit4Id, 0), (choose1Id, 1), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit1Id, 0), (deposit2Id, 0), (deposit4Id, 0), (choose1Id, 5), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit2Id, 0)
                          , (deposit4Id, 0)
                          , (choose1Id, 5)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing choose1Id 5 1
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 1), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit2Id, 0)
                          , (deposit4Id, 0)
                          , (choose1Id, 1)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (deposit2Id, 0), (deposit4Id, 0), (choose2Id, 0), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit1Id, 0), (deposit2Id, 0), (deposit4Id, 0), (choose2Id, 0), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 7), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit2Id, 0)
                          , (deposit4Id, 0)
                          , (choose2Id, 0)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 1), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit2Id, 0)
                          , (deposit4Id, 0)
                          , (choose2Id, 0)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          , (choose1Id, 1)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 31}, POSIXTime{getPOSIXTime = 31}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (deposit2Id, 0), (choose2Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 31}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 36}, POSIXTime{getPOSIXTime = 36}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (deposit2Id, 0), (choose2Id, 0), (deposit3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 36}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues =
                  toAM $ Map{unMap = [(deposit1Id, 0), (deposit2Id, 0), (choose2Id, 0), (deposit3Id, 0), (choose1Id, 5), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (deposit3Id, 0)
                          , (choose1Id, 5)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (deposit3Id, 0)
                          , (choose1Id, 5)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing choose1Id 5 1
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 1), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (deposit3Id, 0)
                          , (choose1Id, 1)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (deposit2Id, 0), (choose2Id, 0), (deposit3Id, 0), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit1Id, 0), (deposit2Id, 0), (choose2Id, 0), (deposit3Id, 0), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 7), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (deposit3Id, 0)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (deposit3Id, 0)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          , (choose1Id, 5)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 36}, POSIXTime{getPOSIXTime = 36}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            ]
        , txOutPayments = [Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (deposit2Id, 0), (choose2Id, 0), (deposit4Id, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 36}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            ]
        , txOutPayments = [Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 1)]}
              , boundValues =
                  toAM $ Map{unMap = [(deposit1Id, 0), (deposit2Id, 0), (choose2Id, 0), (deposit4Id, 5), (choose1Id, 1), (xId, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                10
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (deposit4Id, 5)
                          , (choose1Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                6
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 1), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (deposit4Id, 5)
                          , (choose1Id, 1)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                10
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing choose1Id 5 1
            , TransactionShadowing xId 5 5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 1), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (deposit4Id, 5)
                          , (choose1Id, 1)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            ]
        , txOutPayments = [Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (deposit2Id, 0), (choose2Id, 0), (deposit4Id, 5), (xId, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                12
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit1Id, 0), (deposit2Id, 0), (choose2Id, 0), (deposit4Id, 5), (xId, 5), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                12
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 7), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (deposit4Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                12
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing xId 5 5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (deposit4Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          , (choose1Id, 5)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 31}, POSIXTime{getPOSIXTime = 31}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (deposit2Id, 0), (choose2Id, 0), (notify1Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 31}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 36}, POSIXTime{getPOSIXTime = 36}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (deposit2Id, 0), (choose2Id, 0), (notify1Id, 0), (deposit3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 36}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues =
                  toAM $
                    Map
                      { unMap = [(deposit1Id, 0), (deposit2Id, 0), (choose2Id, 0), (notify1Id, 0), (deposit3Id, 0), (choose1Id, 5), (xId, 0)]
                      }
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit3Id, 0)
                          , (choose1Id, 5)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit3Id, 0)
                          , (choose1Id, 5)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing choose1Id 5 5
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit3Id, 0)
                          , (choose1Id, 5)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues =
                  toAM $ Map{unMap = [(deposit1Id, 0), (deposit2Id, 0), (choose2Id, 0), (notify1Id, 0), (deposit3Id, 0), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit3Id, 0)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 7), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit3Id, 0)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit3Id, 0)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          , (choose1Id, 5)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 36}, POSIXTime{getPOSIXTime = 36}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            ]
        , txOutPayments = [Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit1Id, 0), (deposit2Id, 0), (choose2Id, 0), (notify1Id, 0), (deposit4Id, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 36}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            ]
        , txOutPayments = [Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues =
                  toAM $
                    Map
                      { unMap = [(deposit1Id, 0), (deposit2Id, 0), (choose2Id, 0), (notify1Id, 0), (deposit4Id, 5), (choose1Id, 5), (xId, 5)]
                      }
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                6
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 1)]}
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit4Id, 5)
                          , (choose1Id, 1)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                10
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit4Id, 5)
                          , (choose1Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                10
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing choose1Id 5 5
            , TransactionShadowing xId 5 5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit4Id, 5)
                          , (choose1Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            ]
        , txOutPayments = [Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues =
                  toAM $ Map{unMap = [(deposit1Id, 0), (deposit2Id, 0), (choose2Id, 0), (notify1Id, 0), (deposit4Id, 5), (xId, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                12
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit4Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                12
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 7), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit4Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                12
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing xId 5 5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit1Id, 0)
                          , (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit4Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          , (choose1Id, 5)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 31}, POSIXTime{getPOSIXTime = 31}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = []}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 31}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 36}, POSIXTime{getPOSIXTime = 36}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = []}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (deposit3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 36}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (deposit3Id, 0), (choose1Id, 5), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues =
                  toAM $ Map{unMap = [(deposit2Id, 0), (deposit3Id, 0), (choose1Id, 5), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                1
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 1), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit2Id, 0), (deposit3Id, 0), (choose1Id, 1), (xId, 0), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                1
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing choose1Id 1 5
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit2Id, 0), (deposit3Id, 0), (choose1Id, 5), (xId, 0), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (deposit3Id, 0), (choose2Id, 0), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues =
                  toAM $ Map{unMap = [(deposit2Id, 0), (deposit3Id, 0), (choose2Id, 0), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 7), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit2Id, 0), (deposit3Id, 0), (choose2Id, 0), (xId, 0), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit3Id, 0)
                          , (choose2Id, 0)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          , (choose1Id, 5)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 36}, POSIXTime{getPOSIXTime = 36}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = []}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (deposit4Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 36}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (deposit4Id, 0), (choose1Id, 5), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues =
                  toAM $ Map{unMap = [(deposit2Id, 0), (deposit4Id, 0), (choose1Id, 5), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                1
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 1), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit2Id, 0), (deposit4Id, 0), (choose1Id, 1), (xId, 0), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing choose1Id 5 5
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit2Id, 0), (deposit4Id, 0), (choose1Id, 5), (xId, 0), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (deposit4Id, 0), (choose2Id, 0), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues =
                  toAM $ Map{unMap = [(deposit2Id, 0), (deposit4Id, 0), (choose2Id, 0), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 7), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit2Id, 0), (deposit4Id, 0), (choose2Id, 0), (xId, 0), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit4Id, 0)
                          , (choose2Id, 0)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          , (choose1Id, 5)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 31}, POSIXTime{getPOSIXTime = 31}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (choose2Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 31}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 36}, POSIXTime{getPOSIXTime = 36}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (choose2Id, 0), (deposit3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 36}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (choose2Id, 0), (deposit3Id, 0), (choose1Id, 5), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit2Id, 0), (choose2Id, 0), (deposit3Id, 0), (choose1Id, 5), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (deposit3Id, 0)
                          , (choose1Id, 5)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing choose1Id 5 5
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (deposit3Id, 0)
                          , (choose1Id, 5)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (choose2Id, 0), (deposit3Id, 0), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues =
                  toAM $ Map{unMap = [(deposit2Id, 0), (choose2Id, 0), (deposit3Id, 0), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 7), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit2Id, 0), (choose2Id, 0), (deposit3Id, 0), (xId, 0), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 1), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (deposit3Id, 0)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          , (choose1Id, 1)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 36}, POSIXTime{getPOSIXTime = 36}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            ]
        , txOutPayments = [Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (choose2Id, 0), (deposit4Id, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 36}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            ]
        , txOutPayments = [Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 1)]}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (choose2Id, 0), (deposit4Id, 5), (choose1Id, 1), (xId, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                10
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit2Id, 0), (choose2Id, 0), (deposit4Id, 5), (choose1Id, 5), (xId, 5), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                10
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (deposit4Id, 5)
                          , (choose1Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                10
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing choose1Id 5 1
            , TransactionShadowing xId 5 5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 1), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (deposit4Id, 5)
                          , (choose1Id, 1)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            ]
        , txOutPayments = [Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (choose2Id, 0), (deposit4Id, 5), (xId, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                12
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues =
                  toAM $ Map{unMap = [(deposit2Id, 0), (choose2Id, 0), (deposit4Id, 5), (xId, 5), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                12
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 7), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit2Id, 0), (choose2Id, 0), (deposit4Id, 5), (xId, 5), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                12
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing xId 5 5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 1), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (deposit4Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          , (choose1Id, 1)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 31}, POSIXTime{getPOSIXTime = 31}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (choose2Id, 0), (notify1Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 31}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 36}, POSIXTime{getPOSIXTime = 36}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (choose2Id, 0), (notify1Id, 0), (deposit3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 36}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 1)]}
              , boundValues =
                  toAM $ Map{unMap = [(deposit2Id, 0), (choose2Id, 0), (notify1Id, 0), (deposit3Id, 0), (choose1Id, 1), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                1
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 1)]}
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit3Id, 0)
                          , (choose1Id, 1)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit3Id, 0)
                          , (choose1Id, 5)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing choose1Id 5 5
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit3Id, 0)
                          , (choose1Id, 5)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (choose2Id, 0), (notify1Id, 0), (deposit3Id, 0), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit2Id, 0), (choose2Id, 0), (notify1Id, 0), (deposit3Id, 0), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 7), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit3Id, 0)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 1), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit3Id, 0)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          , (choose1Id, 1)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 36}, POSIXTime{getPOSIXTime = 36}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            ]
        , txOutPayments = [Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (choose2Id, 0), (notify1Id, 0), (deposit4Id, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 36}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            ]
        , txOutPayments = [Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 1)]}
              , boundValues =
                  toAM $ Map{unMap = [(deposit2Id, 0), (choose2Id, 0), (notify1Id, 0), (deposit4Id, 5), (choose1Id, 1), (xId, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                10
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit4Id, 5)
                          , (choose1Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                10
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit4Id, 5)
                          , (choose1Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                10
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing choose1Id 5 5
            , TransactionShadowing xId 5 5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit4Id, 5)
                          , (choose1Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            ]
        , txOutPayments = [Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (choose2Id, 0), (notify1Id, 0), (deposit4Id, 5), (xId, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                12
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit2Id, 0), (choose2Id, 0), (notify1Id, 0), (deposit4Id, 5), (xId, 5), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                12
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 7), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit4Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                12
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing xId 5 5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit4Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          , (choose1Id, 5)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 31}, POSIXTime{getPOSIXTime = 31}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = []}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (deposit1Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 31}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 36}, POSIXTime{getPOSIXTime = 36}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = []}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (deposit1Id, 0), (deposit3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 36}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 1)]}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (deposit1Id, 0), (deposit3Id, 0), (choose1Id, 1), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit2Id, 0), (deposit1Id, 0), (deposit3Id, 0), (choose1Id, 5), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                1
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 1), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (deposit3Id, 0)
                          , (choose1Id, 1)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing choose1Id 5 1
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 1), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (deposit3Id, 0)
                          , (choose1Id, 1)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (deposit1Id, 0), (deposit3Id, 0), (choose2Id, 0), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit2Id, 0), (deposit1Id, 0), (deposit3Id, 0), (choose2Id, 0), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 7), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (deposit3Id, 0)
                          , (choose2Id, 0)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (deposit3Id, 0)
                          , (choose2Id, 0)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          , (choose1Id, 5)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 36}, POSIXTime{getPOSIXTime = 36}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = []}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (deposit1Id, 0), (deposit4Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 36}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 1)]}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (deposit1Id, 0), (deposit4Id, 0), (choose1Id, 1), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit2Id, 0), (deposit1Id, 0), (deposit4Id, 0), (choose1Id, 5), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (deposit4Id, 0)
                          , (choose1Id, 5)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing choose1Id 5 5
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (deposit4Id, 0)
                          , (choose1Id, 5)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (deposit1Id, 0), (deposit4Id, 0), (choose2Id, 0), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit2Id, 0), (deposit1Id, 0), (deposit4Id, 0), (choose2Id, 0), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 7), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (deposit4Id, 0)
                          , (choose2Id, 0)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyNoe ada 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (deposit4Id, 0)
                          , (choose2Id, 0)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          , (choose1Id, 5)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 31}, POSIXTime{getPOSIXTime = 31}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (deposit1Id, 0), (choose2Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 31}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 36}, POSIXTime{getPOSIXTime = 36}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (deposit1Id, 0), (choose2Id, 0), (deposit3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 36}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 1)]}
              , boundValues =
                  toAM $ Map{unMap = [(deposit2Id, 0), (deposit1Id, 0), (choose2Id, 0), (deposit3Id, 0), (choose1Id, 1), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (deposit3Id, 0)
                          , (choose1Id, 5)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                1
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 1), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (deposit3Id, 0)
                          , (choose1Id, 1)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing choose1Id 5 1
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 1), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (deposit3Id, 0)
                          , (choose1Id, 1)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (deposit1Id, 0), (choose2Id, 0), (deposit3Id, 0), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit2Id, 0), (deposit1Id, 0), (choose2Id, 0), (deposit3Id, 0), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 7), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (deposit3Id, 0)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 1), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (deposit3Id, 0)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          , (choose1Id, 1)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 36}, POSIXTime{getPOSIXTime = 36}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            ]
        , txOutPayments = [Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (deposit1Id, 0), (choose2Id, 0), (deposit4Id, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 36}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            ]
        , txOutPayments = [Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues =
                  toAM $ Map{unMap = [(deposit2Id, 0), (deposit1Id, 0), (choose2Id, 0), (deposit4Id, 5), (choose1Id, 5), (xId, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                10
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (deposit4Id, 5)
                          , (choose1Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                10
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (deposit4Id, 5)
                          , (choose1Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                10
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing choose1Id 5 5
            , TransactionShadowing xId 5 5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (deposit4Id, 5)
                          , (choose1Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            ]
        , txOutPayments = [Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (deposit1Id, 0), (choose2Id, 0), (deposit4Id, 5), (xId, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                12
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues =
                  toAM $
                    Map{unMap = [(deposit2Id, 0), (deposit1Id, 0), (choose2Id, 0), (deposit4Id, 5), (xId, 5), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                12
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 7), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (deposit4Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 26}, POSIXTime{getPOSIXTime = 26})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                12
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing xId 5 5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (deposit4Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          , (choose1Id, 5)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 31}, POSIXTime{getPOSIXTime = 31}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (deposit1Id, 0), (choose2Id, 0), (notify1Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 31}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 36}, POSIXTime{getPOSIXTime = 36}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (deposit1Id, 0), (choose2Id, 0), (notify1Id, 0), (deposit3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 36}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues =
                  toAM $
                    Map
                      { unMap = [(deposit2Id, 0), (deposit1Id, 0), (choose2Id, 0), (notify1Id, 0), (deposit3Id, 0), (choose1Id, 5), (xId, 0)]
                      }
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit3Id, 0)
                          , (choose1Id, 5)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit3Id, 0)
                          , (choose1Id, 5)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                5
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing choose1Id 5 1
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 1), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit3Id, 0)
                          , (choose1Id, 1)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            ]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues =
                  toAM $ Map{unMap = [(deposit2Id, 0), (deposit1Id, 0), (choose2Id, 0), (notify1Id, 0), (deposit3Id, 0), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit3Id, 0)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 7), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit3Id, 0)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                7
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit3Id, 0)
                          , (xId, 0)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          , (choose1Id, 5)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 36}, POSIXTime{getPOSIXTime = 36}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            ]
        , txOutPayments = [Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit2Id, 0), (deposit1Id, 0), (choose2Id, 0), (notify1Id, 0), (deposit4Id, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 36}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            ]
        , txOutPayments = [Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues =
                  toAM $
                    Map
                      { unMap = [(deposit2Id, 0), (deposit1Id, 0), (choose2Id, 0), (notify1Id, 0), (deposit4Id, 5), (choose1Id, 5), (xId, 5)]
                      }
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                10
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit4Id, 5)
                          , (choose1Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                6
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 1), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit4Id, 5)
                          , (choose1Id, 1)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                10
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing choose1Id 5 1
            , TransactionShadowing xId 5 5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 1), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit4Id, 5)
                          , (choose1Id, 1)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            ]
        , txOutPayments = [Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues =
                  toAM $ Map{unMap = [(deposit2Id, 0), (deposit1Id, 0), (choose2Id, 0), (notify1Id, 0), (deposit4Id, 5), (xId, 5)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 46}, POSIXTime{getPOSIXTime = 46}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                12
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7)]}
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit4Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                12
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 7), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit4Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party1
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs =
              [ NormalInput
                  ( IDeposit
                      party1
                      party2
                      ada
                      0
                  )
              ]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy ada 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partySten ada 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit
                party1
                party1
                ada
                0
            , TransactionNonPositiveDeposit
                party2
                party1
                ada
                0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy ada 0
            , TransactionPartialPay
                party1
                (Party partyCy)
                token2
                0
                12
            , TransactionNonPositivePay
                party2
                (Party party1)
                token2
                0
            , TransactionShadowing xId 5 5
            ]
        , txOutPayments =
            [ Payment
                party1
                (Party partyCy)
                token2
                0
            , Payment partyNoe (Party partyNoe) ada 5
            ]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices =
                  toAM $
                    Map
                      { unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]
                      }
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit4Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          , (choose1Id, 5)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ]

-- | A list of test cases and results that should fail.
invalids :: [(POSIXTime, [TransactionInput], TransactionOutput)]
invalids =
  [
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 36}, POSIXTime{getPOSIXTime = 36}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings = [TransactionNonPositiveDeposit partyCy partyCy token2 0]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = []}
              , boundValues = toAM $ Map{unMap = [(deposit3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 36}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 3)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 41}, POSIXTime{getPOSIXTime = 41}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings = [TransactionNonPositiveDeposit partyCy partyCy token2 0]
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 1)]}
              , boundValues = toAM $ Map{unMap = [(deposit3Id, 0), (choose1Id, 1), (xId, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 41}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten token1 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 16}, POSIXTime{getPOSIXTime = 16}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy token1 0
            , TransactionPartialPay party1 (Party partyCy) token2 0 5
            ]
        , txOutPayments = [Payment party1 (Party partyCy) ada 0]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5)]}
              , boundValues = toAM $ Map{unMap = [(deposit3Id, 0), (choose1Id, 5), (xId, 0), (deposit5Id, 0), (labId, 10)]}
              , minTime = POSIXTime{getPOSIXTime = 46}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten token1 3)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 51}, POSIXTime{getPOSIXTime = 51}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy token1 0
            , TransactionPartialPay party1 (Party partyCy) token2 0 5
            , TransactionNonPositivePay party2 (Party party1) token2 0
            ]
        , txOutPayments = [Payment party1 (Party partyCy) ada 0]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]}
              , boundValues =
                  toAM $ Map{unMap = [(deposit3Id, 0), (choose1Id, 5), (xId, 0), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partyCy token2 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IDeposit partyCy partySten token1 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 21}, POSIXTime{getPOSIXTime = 21})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit partyCy partyCy token2 0
            , TransactionNonPositiveDeposit partySten partyCy token1 0
            , TransactionPartialPay party1 (Party partyCy) token2 0 5
            , TransactionNonPositivePay party2 (Party party1) token2 0
            , TransactionShadowing choose1Id 5 5
            , TransactionShadowing xId 0 0
            ]
        , txOutPayments = [Payment party1 (Party partyCy) ada 0]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]}
              , boundValues =
                  toAM $ Map{unMap = [(deposit3Id, 0), (choose1Id, 5), (xId, 0), (deposit5Id, 0), (labId, 10), (choose3Id, 0)]}
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit party1 party1 token1 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit party1 party2 token1 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy token1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partySten token1 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 1)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 11}, POSIXTime{getPOSIXTime = 11}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit party1 party1 token1 0
            , TransactionNonPositiveDeposit party2 party1 token1 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy token1 0
            , TransactionPartialPay party1 (Party partyCy) token2 0 12
            , TransactionNonPositivePay party2 (Party party1) token2 0
            ]
        , txOutPayments = [Payment party1 (Party partyCy) ada 0, Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 7), (choiceBeByPartyCy, 1)]}
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit4Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 51}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6}), txInputs = []}
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit party1 party1 token1 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit party1 party2 token1 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyNoe partyCy token1 5)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 7)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IDeposit partyCy partySten token1 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByPartyCy 11)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 6}, POSIXTime{getPOSIXTime = 6})
          , txInputs = [NormalInput (IChoice choiceBeByParty1 5)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 56}, POSIXTime{getPOSIXTime = 56}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings =
            [ TransactionNonPositiveDeposit party1 party1 token1 0
            , TransactionNonPositiveDeposit party2 party1 token1 0
            , TransactionShadowing choose2Id 0 0
            , TransactionNonPositiveDeposit partySten partyCy token1 0
            , TransactionPartialPay party1 (Party partyCy) token2 0 12
            , TransactionNonPositivePay party2 (Party party1) token2 0
            , TransactionShadowing xId 5 5
            ]
        , txOutPayments = [Payment party1 (Party partyCy) ada 0, Payment partyNoe (Party partyNoe) ada 5]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "be" party1, 5), (choiceBeByPartyCy, 1)]}
              , boundValues =
                  toAM $
                    Map
                      { unMap =
                          [ (deposit2Id, 0)
                          , (deposit1Id, 0)
                          , (choose2Id, 0)
                          , (notify1Id, 0)
                          , (deposit4Id, 5)
                          , (xId, 5)
                          , (deposit5Id, 0)
                          , (labId, 10)
                          , (choose3Id, 0)
                          , (choose1Id, 5)
                          ]
                      }
              , minTime = POSIXTime{getPOSIXTime = 56}
              }
        , txOutContract = Close
        }
    )
  ]
