{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Web.Contract.APISpec (
  spec,
) where

import Cardano.Api qualified as C
import Data.Aeson qualified as A
import Data.Proxy (Proxy (Proxy))
import Language.Marlowe.Runtime.Web.Contract.API (PostContractsRequest)
import Language.Marlowe.Runtime.Web.Gen ()
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck qualified as Hspec.QuickCheck
import Test.QuickCheck ((===))
import Language.Marlowe.Runtime.Web.Core.Tx (TransactionUnspentOutput)

spec :: Spec
spec = describe "Contract.API" do
  Hspec.QuickCheck.prop "PostContractsRequest encoding/decoding is reversible" $ \(request :: PostContractsRequest) -> do
    let
      encoded = A.encode request
      decoded = A.eitherDecode encoded
    decoded === Right request

  Hspec.QuickCheck.prop "TransactionUnspentOutput CBOR encoding/decoding is reversible" $ \(tuo :: TransactionUnspentOutput C.ConwayEra) -> do
    let
      encoded = C.serialiseToCBOR tuo
      decoded = C.deserialiseFromCBOR (C.proxyToAsType $ Proxy @(TransactionUnspentOutput C.ConwayEra)) encoded
    decoded === Right tuo


