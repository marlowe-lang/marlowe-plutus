-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Reference golden output for a contract with negative deposits.
module Spec.Marlowe.Semantics.Golden.Negative (
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
  TransactionWarning (TransactionNonPositiveDeposit),
 )
import Language.Marlowe.Plutus.Semantics.Types (
  Action (Choice, Deposit, Notify),
  Bound (..),
  Case (Case),
  ChoiceId (..),
  Contract (Close, Let, When),
  Input (NormalInput),
  InputContent (IChoice, IDeposit, INotify),
  Observation (ValueEQ, ValueGT, ValueLT),
  Party,
  Payee (Party),
  State (State, accounts, boundValues, choices, minTime),
  Value (ChoiceValue, Constant),
  ada,
  mkValueIdUtf8, mkRoleUtf8, ValueId,
 )

import PlutusLedgerApi.V2 (POSIXTime (..))

import qualified Language.Marlowe.Plutus.AssocMap as AM (Map, unsafeFromList)

party :: Party
party = mkRoleUtf8 "Party"

counterparty :: Party
counterparty = mkRoleUtf8 "Counterparty"

positive :: ValueId
positive = mkValueIdUtf8 "Positive"

neutral :: ValueId
neutral = mkValueIdUtf8 "Neutral"

negative :: ValueId
negative = mkValueIdUtf8 "Negative"

-- | The Zero-Coupon Bond contract.
contract :: Contract
contract =
  let initial = Constant 2_000_000
      choice = ChoiceId "Deposit" party
      value = ChoiceValue choice
      initialDeadline = 1_000
      choiceDeadline = 2_000
      notifyDeadline = 3_000
      depositDeadline = 4_000
   in When
        [ Case (Deposit counterparty counterparty ada initial) $
            When
              [ Case (Choice choice [Bound (-1) 1]) $
                  When
                    [ Case
                        (Deposit party counterparty ada value)
                        Close
                    , Case (Notify $ ValueGT value $ Constant 0) $
                        Let positive (Constant 1) $
                          When
                            [ Case
                                (Deposit party counterparty ada value)
                                Close
                            ]
                            depositDeadline
                            Close
                    , Case (Notify $ ValueEQ value $ Constant 0) $
                        Let neutral (Constant 1) $
                          When
                            [ Case
                                (Deposit party party ada value)
                                Close
                            ]
                            depositDeadline
                            Close
                    , Case (Notify $ ValueLT value $ Constant 0) $
                        Let negative (Constant 1) $
                          When
                            [ Case
                                (Deposit counterparty party ada value)
                                Close
                            ]
                            depositDeadline
                            Close
                    ]
                    notifyDeadline
                    Close
              ]
              choiceDeadline
              Close
        ]
        initialDeadline
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
    , [TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 1_000}, POSIXTime{getPOSIXTime = 1_000}), txInputs = []}]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = []}
              , boundValues = toAM $ Map{unMap = []}
              , minTime = POSIXTime{getPOSIXTime = 1_000}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IDeposit counterparty counterparty ada 2_000_000)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 2_000}, POSIXTime{getPOSIXTime = 2_000}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments = [Payment counterparty (Party counterparty) ada 2_000_000]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = []}
              , boundValues = toAM $ Map{unMap = []}
              , minTime = POSIXTime{getPOSIXTime = 2_000}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IDeposit counterparty counterparty ada 2_000_000)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IChoice (ChoiceId "Deposit" party) 0)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 3_000}, POSIXTime{getPOSIXTime = 3_000}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments = [Payment counterparty (Party counterparty) ada 2_000_000]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "Deposit" party, 0)]}
              , boundValues = toAM $ Map{unMap = []}
              , minTime = POSIXTime{getPOSIXTime = 3_000}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IDeposit counterparty counterparty ada 2_000_000)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IChoice (ChoiceId "Deposit" party) 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IDeposit party counterparty ada 0)]
          }
      ]
    , TransactionOutput
        { txOutWarnings = [TransactionNonPositiveDeposit counterparty party ada 0]
        , txOutPayments = [Payment counterparty (Party counterparty) ada 2_000_000]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "Deposit" party, 0)]}
              , boundValues = toAM $ Map{unMap = []}
              , minTime = POSIXTime{getPOSIXTime = 0}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IDeposit counterparty counterparty ada 2_000_000)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IChoice (ChoiceId "Deposit" party) 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 4_000}, POSIXTime{getPOSIXTime = 4_000}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments = [Payment counterparty (Party counterparty) ada 2_000_000]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "Deposit" party, 1)]}
              , boundValues = toAM $ Map{unMap = [(positive, 1)]}
              , minTime = POSIXTime{getPOSIXTime = 4_000}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IDeposit counterparty counterparty ada 2_000_000)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IChoice (ChoiceId "Deposit" party) 1)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IDeposit party counterparty ada 1)]
          }
      ]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments =
            [Payment counterparty (Party counterparty) ada 2_000_000, Payment party (Party party) ada 1]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "Deposit" party, 1)]}
              , boundValues = toAM $ Map{unMap = [(positive, 1)]}
              , minTime = POSIXTime{getPOSIXTime = 0}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IDeposit counterparty counterparty ada 2_000_000)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IChoice (ChoiceId "Deposit" party) 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 4_000}, POSIXTime{getPOSIXTime = 4_000}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments = [Payment counterparty (Party counterparty) ada 2_000_000]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "Deposit" party, 0)]}
              , boundValues = toAM $ Map{unMap = [(neutral, 1)]}
              , minTime = POSIXTime{getPOSIXTime = 4_000}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IDeposit counterparty counterparty ada 2_000_000)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IChoice (ChoiceId "Deposit" party) 0)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IDeposit party party ada 0)]
          }
      ]
    , TransactionOutput
        { txOutWarnings = [TransactionNonPositiveDeposit party party ada 0]
        , txOutPayments = [Payment counterparty (Party counterparty) ada 2_000_000]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "Deposit" party, 0)]}
              , boundValues = toAM $ Map{unMap = [(neutral, 1)]}
              , minTime = POSIXTime{getPOSIXTime = 0}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IDeposit counterparty counterparty ada 2_000_000)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IChoice (ChoiceId "Deposit" party) (-1))]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 4_000}, POSIXTime{getPOSIXTime = 4_000}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments = [Payment counterparty (Party counterparty) ada 2_000_000]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "Deposit" party, -1)]}
              , boundValues = toAM $ Map{unMap = [(negative, 1)]}
              , minTime = POSIXTime{getPOSIXTime = 4_000}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IDeposit counterparty counterparty ada 2_000_000)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IChoice (ChoiceId "Deposit" party) (-1))]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IDeposit counterparty party ada (-1))]
          }
      ]
    , TransactionOutput
        { txOutWarnings = [TransactionNonPositiveDeposit party counterparty ada (-1)]
        , txOutPayments = [Payment counterparty (Party counterparty) ada 2_000_000]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = [(ChoiceId "Deposit" party, -1)]}
              , boundValues = toAM $ Map{unMap = [(negative, 1)]}
              , minTime = POSIXTime{getPOSIXTime = 0}
              }
        , txOutContract = Close
        }
    )
  ]

-- | A list of test cases and results that should fail.
invalids :: [(POSIXTime, [TransactionInput], TransactionOutput)]
invalids =
  []
