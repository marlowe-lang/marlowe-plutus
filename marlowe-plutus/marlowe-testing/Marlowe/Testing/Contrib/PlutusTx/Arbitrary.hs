{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Generate random data for Plutus tests.
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
module Marlowe.Testing.Contrib.PlutusTx.Arbitrary (
  arbitraryAnyCurrencySymbol,
  arbitraryBuiltinByteString,
  arbitraryFibonacci,
  arbitraryNonAdaCurrencySymbol,
  arbitraryPlutusTxAssocMap,
  shrinkBuiltinByteString,
) where

import PlutusLedgerApi.V2 (
  BuiltinData (..),
  Data (..),
  Datum (..),
  DatumHash (..),
  Extended (..),
  Interval (..),
  LowerBound (..),
  OutputDatum (..),
  Redeemer (..),
  ScriptContext (..),
  ScriptPurpose (..),
  TxId (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  TxOutRef (..),
  UpperBound (..),
  Value (..),
  adaSymbol,
  adaToken,
  singleton,
  toBuiltin,
 )
import PlutusTx.Builtins (BuiltinByteString, lengthOfByteString)
import Marlowe.Testing.Contrib.Integer.Arbitrary (arbitraryPositiveInteger, arbitraryInteger)

import Test.Tasty.QuickCheck (
  Arbitrary (..),
  Gen,
  chooseInt,
  frequency,
  getNonNegative,
  listOf,
  resize,
  sized,
  suchThat,
  vectorOf,
 )

import qualified Data.ByteString as BS (ByteString, pack)
import qualified Data.ByteString.Char8 as BS8 (pack)
import qualified PlutusTx.AssocMap as PlutusTxAM
import qualified PlutusTx.List as List
import PlutusLedgerApi.V1 (PubKeyHash, CurrencySymbol, TokenName (TokenName), POSIXTime (POSIXTime), Address(..), Credential (PubKeyCredential, ScriptCredential), StakingCredential (StakingHash), ScriptHash)
import Language.Marlowe.Plutus.Semantics.Types (unsafeMkCurrencySymbolHex, mkTokenNameByteString)

arbitraryPlutusTxAssocMap :: (Eq k) => Gen k -> Gen v -> Gen (PlutusTxAM.Map k v)
arbitraryPlutusTxAssocMap arbitraryKey arbitraryValue =
  PlutusTxAM.unsafeFromList . List.nubBy (\(key1, _) (key2, _) -> key1 == key2)
    <$> listOf ((,) <$> arbitraryKey <*> arbitraryValue)

-- | Generate an arbitrary bytestring of specified length.
arbitraryBuiltinByteString :: Int -> Gen BuiltinByteString
arbitraryBuiltinByteString n = toBuiltin . BS.pack <$> vectorOf n arbitrary

-- | Shrink a byte string.
shrinkBuiltinByteString :: (a -> BuiltinByteString) -> [a] -> a -> [a]
shrinkBuiltinByteString f universe selected =
  filter
    (\candidate -> lengthOfByteString (f candidate) > 0 && lengthOfByteString (f candidate) < lengthOfByteString (f selected))
    universe

instance Arbitrary Address where
  arbitrary = Address <$> arbitrary <*> arbitrary

instance Arbitrary Credential where
  arbitrary = do
    frequency
      [ (39, PubKeyCredential <$> arbitrary)
      , (1, ScriptCredential <$> arbitrary)
      ]

instance Arbitrary StakingCredential where
  arbitrary = StakingHash <$> arbitrary

instance Arbitrary ScriptHash where
  arbitrary = arbitraryFibonacci randomScriptHashes

-- | Some validator hashes.
randomScriptHashes :: [ScriptHash]
randomScriptHashes =
  [ "03e718204caac168d55e891f87b2b01da688e4501ce560ae613fa7e7"
  , "1b4a1ddee561fdce46d70d07976d3ef2f4c03985195906c08f547249"
  , "2f7bbc97515f6313cd667ac7345b5530241f2b0300c46b12f083ef46"
  , "40db3573f94cda9b7cc80de24517df78a0031d1e4a42cea9127cc730"
  , "592790cff4ffd7ad91de59e6af421faed0a703a03445beb8efb17fbf"
  , "6487e8994d0e4a9b509c2309a827dc18fb405749d7d4f3fce3ea03f7"
  , "657e1b61c43d544f1d628b758226addaadf4eb4b6b411fbbc547ba7c"
  , "7b6fcf537493391fd4536a5528d3eea46f9aa63d41bdae6506ecf58d"
  , "8169906d393fdd404a60b694a3558b0ddb51a5e6c018698054f92f0a"
  , "85f2490b7284c0440882b3d9a4f7a91b9e0946ec51ee8cce40ad423b"
  , "98454636923133bf58517ec1285b4fe4d90d1b1a71263f882ea6911d"
  , "a2bd7ddd1d11c7f4994fa7f41c2781c750525705f9c259f97cb27d0e"
  , "c5b4c54ec387ad8250b183a0d0d181617bb18bcf2eccc0f27fe7aa23"
  , "d877b83fee4a52bd72269ece77d78549fa64e111aa0e20cd4a1c471b"
  -- , "d877b83fee4a52bd72269ece77d78549fa64e111aa0e20cd4a1c47"
  -- , "d877b83fee4a52bd72269ece77d78549fa64e111aa0e20cd4a1c470908"
  ]


instance Arbitrary BS.ByteString where
  arbitrary = BS8.pack <$> arbitrary

instance Arbitrary BuiltinByteString where
  arbitrary = toBuiltin . BS.pack <$> arbitrary

instance Arbitrary BuiltinData where
  arbitrary = BuiltinData <$> arbitrary

instance Arbitrary Data where
  arbitrary = sized \size ->
    if size <= 0
      then
        frequency
          [ (1, I <$> arbitrary)
          , (2, B <$> arbitrary)
          ]
      else
        frequency
          [
            ( 1
            , do
                subDataCount <- chooseInt (0, floor $ sqrt @Double $ fromIntegral size)
                let subDatumSize = size `quot` subDataCount
                Constr . getNonNegative <$> arbitrary <*> vectorOf subDataCount (resize subDatumSize arbitrary)
            )
          ,
            ( 2
            , do
                subDataCount <- chooseInt (0, floor $ sqrt @Double $ fromIntegral size)
                let subDatumSize = size `quot` (subDataCount * 2)
                Map <$> vectorOf subDataCount (resize subDatumSize arbitrary)
            )
          ,
            ( 5
            , do
                subDataCount <- chooseInt (0, floor $ sqrt @Double $ fromIntegral size)
                let subDatumSize = size `quot` subDataCount
                List <$> vectorOf subDataCount (resize subDatumSize arbitrary)
            )
          , (10, I <$> arbitrary)
          , (20, B <$> arbitrary)
          ]

instance Arbitrary Datum where
  arbitrary = Datum <$> arbitrary

instance Arbitrary DatumHash where
  arbitrary = DatumHash <$> arbitrary

instance (Arbitrary a) => Arbitrary (Extended a) where
  arbitrary =
    frequency
      [ (1, pure NegInf)
      , (9, Finite <$> arbitrary)
      , (1, pure PosInf)
      ]

instance (Arbitrary a) => Arbitrary (Interval a) where
  arbitrary = Interval <$> arbitrary <*> arbitrary

instance (Arbitrary a) => Arbitrary (LowerBound a) where
  arbitrary = LowerBound <$> arbitrary <*> arbitrary

instance Arbitrary Redeemer where
  arbitrary = Redeemer <$> arbitrary

instance Arbitrary ScriptContext where
  arbitrary = ScriptContext <$> arbitrary <*> (Spending <$> arbitrary)

instance Arbitrary ScriptPurpose where
  arbitrary =
    frequency
      [ (2, Minting <$> arbitraryNonAdaCurrencySymbol)
      , (8, Spending <$> arbitrary)
      ]

instance Arbitrary TxId where
  arbitrary = TxId <$> arbitraryBuiltinByteString 32

instance Arbitrary POSIXTime where
  arbitrary = POSIXTime <$> arbitraryInteger
  shrink (POSIXTime i) = POSIXTime <$> shrink i

instance Arbitrary TxInfo where
  arbitrary =
    do
      txInfoInputs <- arbitrary
      txInfoReferenceInputs <- arbitrary
      txInfoOutputs <- arbitrary
      txInfoFee <- singleton adaSymbol adaToken <$> arbitraryPositiveInteger
      txInfoValidRange <- arbitrary
      txInfoSignatories <- arbitrary
      txInfoRedeemers <- arbitraryPlutusTxAssocMap arbitrary arbitrary
      txInfoData <- arbitraryPlutusTxAssocMap arbitrary arbitrary
      let txInfoMint = mempty
          txInfoDCert = mempty
          txInfoWdrl = PlutusTxAM.empty
      txInfoId <- arbitrary
      pure TxInfo{..}

instance Arbitrary TxInInfo where
  arbitrary = TxInInfo <$> arbitrary <*> arbitrary

instance Arbitrary TxOut where
  arbitrary =
    TxOut
      <$> arbitrary
      <*> genValue arbitraryPositiveInteger `suchThat` (not . PlutusTxAM.null . getValue)
      <*> (OutputDatumHash <$> arbitrary)
      <*> pure Nothing

instance Arbitrary TxOutRef where
  arbitrary = TxOutRef <$> arbitrary <*> arbitraryPositiveInteger

instance (Arbitrary a) => Arbitrary (UpperBound a) where
  arbitrary = UpperBound <$> arbitrary <*> arbitrary

instance Arbitrary Value where
  arbitrary = genValue arbitrary

genValue :: Gen Integer -> Gen Value
genValue genQuantity = do
  -- Empty currency symbol can be only paired with empty token.
  -- So we first generate a Value without Ada and then randomly decide whether to add Ada or not.
  withoutAda <- Value <$> arbitraryPlutusTxAssocMap arbitraryNonAdaCurrencySymbol (arbitraryPlutusTxAssocMap arbitrary genQuantity)
  addAda <- arbitrary
  if addAda
    then do
      adaValue <- singleton adaSymbol adaToken <$> genQuantity
      pure $ withoutAda <> adaValue
    else pure withoutAda

-- | Part of the Fibonacci sequence.
fibonaccis :: (Num a) => [a]
fibonaccis = 2 : 3 : zipWith (+) fibonaccis (tail fibonaccis)

-- | Inverse-Fibonacci frequencies.
fibonacciFrequencies :: (Integral a) => [a]
fibonacciFrequencies = (1000000 `div`) <$> fibonaccis

-- | Select an element of a list with probability proportional to inverse-Fibonacci weights.
arbitraryFibonacci :: [a] -> Gen a
arbitraryFibonacci = frequency . zip fibonacciFrequencies . fmap pure

-- FIXME: In the old implementation there were values which were not valid PubKeyHashes
-- Which makes no sens as the ledger will never produce such invalid values and arbitrary
-- should only produce valid values.
-- | Some public key hashes.
randomPubKeyHashes :: [PubKeyHash]
randomPubKeyHashes =
  [ "03e718291f87b2b004caac168d55e81da688e4501ce560ae613fa7e7"
  , "1b4a1ddd07976d3eee561fdce46d70f2f4c03985195906c08f547249"
  , "2f7bbc9ac7345b557515f6313cd66730241f2b0300c46b12f083ef46"
  , "40db357de24517df3f94cda9b7cc8078a0031d1e4a42cea9127cc730"
  , "592790c9e6af421fff4ffd7ad91de5aed0a703a03445beb8efb17fbf"
  , "6487e89309a827dc94d0e4a9b509c218fb405749d7d4f3fce3ea03f7"
  , "657e1b6b758226ad1c43d544f1d628daadf4eb4b6b411fbbc547ba7c"
  , "7b6fcf5a5528d3ee37493391fd4536a46f9aa63d41bdae6506ecf58d"
  , "8169906694a3558bd393fdd404a60b0ddb51a5e6c018698054f92f0a"
  , "85f24903d9a4f7a9b7284c0440882b1b9e0946ec51ee8cce40ad423b"
  , "9845463ec1285b4f6923133bf58517e4d90d1b1a71263f882ea6911d"
  , "a2bd7dd7f41c2781d1d11c7f4994fac750525705f9c259f97cb27d0e"
  , "c5b4c543a0d0d181ec387ad8250b18617bb18bcf2eccc0f27fe7aa23"
  , "d877b83ece77d785fee4a52bd7226949fa64e111aa0e20cd4a1c471b"
  -- , "d877b83fee4a52bd72269ece77d78549fa64e111aa0e20cd4a1c47"
  -- , "d877b83fee4a52bd72269ece77d78549fa64e111aa0e20cd4a1c470908"
  ]

instance Arbitrary PubKeyHash where
  arbitrary = arbitraryFibonacci randomPubKeyHashes

randomCurrencySymbols :: [CurrencySymbol]
randomCurrencySymbols =
  unsafeMkCurrencySymbolHex
    <$> [ "13e78e78c233e131b0cbe4424225d338b7c5ac65e16df0a3e6c9d8f8"
        , "1b9af43b0eaafc42dfaefbbf4e71437af45454c7292a6b6606363741"
        , "23d79373f7d9edbd016c99e21a473f498a2e425491244ecbc663e9d0"
        , "2839d40108e194eced45205c89613df56bd482e07e6c81a1df2b0e9b"
        , "2c60fb96c894b099f1a21ca9cf51c8c46a4672eb9a30b85252e9adb7"
        , "35c100db45fdf04b9317a2c520c2638ead47fd792984f32c9652cbc7"
        , "443d7002ac74be8c3c53f901d95c89c5932ee8946b188ca9f59db24e"
        , "63f3875b161780b82c7706fbc36fe906e54742e9f5b4c68d260e5da9"
        , "64da8cbb98eccc616bb0061efed2717393e4b48d8f78147396f4521f"
        , "66879477b60f46e5c5ad1d1bb124ab5c3d46a3acc9e54b7da4259655"
        , "9019bb7fb44ec03537b61a6f4aa3fd7b1effaf0776c3d449e9c6274e"
        , "9f92753881b398a247e53b6cad08eab0e158cf1ef5df84c7f5766041"
        , "c1f46ec0147542f9bc155805993497ed44150687a41d0a63af3be466"
        , "cc2189d7adde0ed26355fd03e134feb508e5924959b07a53557f285e"
        ]

arbitraryNonAdaCurrencySymbol :: Gen CurrencySymbol
arbitraryNonAdaCurrencySymbol = arbitraryFibonacci randomCurrencySymbols

arbitraryAnyCurrencySymbol :: Gen CurrencySymbol
arbitraryAnyCurrencySymbol = frequency [(1, pure adaSymbol), (9, arbitraryNonAdaCurrencySymbol)]

randomTokenNames :: [TokenName]
randomTokenNames =
  mkTokenNameByteString
    <$> [ "I"
        , "AD"
        , "PIN"
        , "TALE"
        , "RIVER"
        , "METHOD"
        , "REVENUE"
        , ""
        , "POSSIBILITY"
        , "SATISFACTION"
        , "PAYMENT CONCEPT"
        , "OFFICE DEFINITION"
        , "ARTISAN CONVERSATION"
        , "SOFTWARE FEEDBACK METHOD"
        , "INDEPENDENCE EXPLANATION REVENUE"
        , "01234567890ABCDEF01234567890ABCDEF"
        ]

instance Arbitrary TokenName where
  arbitrary = arbitraryFibonacci randomTokenNames
  shrink = shrinkBuiltinByteString (\(TokenName x) -> x) randomTokenNames

