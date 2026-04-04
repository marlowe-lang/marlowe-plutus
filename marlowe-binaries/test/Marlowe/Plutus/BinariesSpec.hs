module Marlowe.Plutus.BinariesSpec
  ( binariesSpec
  ) where

import Test.Hspec
import Test.QuickCheck (property, (===))
import qualified Data.ByteString.Short as SBS

import qualified Marlowe.Plutus.Binaries.Devel as Devel

maxSemanticsSize :: Int
maxSemanticsSize = 12265

binariesSpec :: Spec
binariesSpec = do
  describe "Binary guarantees" do
    describe "Semantics validator" do
      it "Has reasonable size" $
        SBS.length Devel.marloweValidatorBytes `shouldSatisfy` (<= maxSemanticsSize)

      it "Matches golden hash" $
        property $
          Devel.marloweValidatorHash === Devel.marloweValidatorHash

    describe "Payout validator" do
      it "Matches golden hash" $
        property $
          Devel.rolePayoutValidatorHash === Devel.rolePayoutValidatorHash
