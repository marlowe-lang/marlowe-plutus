module Language.Marlowe.Plutus.BinariesSpec
  ( binariesSpec
  ) where

import Test.Hspec
import Test.QuickCheck (property, (===))
import qualified Data.ByteString.Short as SBS

import qualified Language.Marlowe.Plutus.Binaries as Devel

maxSemanticsSize :: Int
maxSemanticsSize = 11500

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
