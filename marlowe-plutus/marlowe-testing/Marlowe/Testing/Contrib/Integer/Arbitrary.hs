module Marlowe.Testing.Contrib.Integer.Arbitrary (
  arbitraryNonnegativeInteger,
  arbitraryPositiveInteger,
  arbitraryInteger,
) where

import Data.Functor ((<&>))
import Test.QuickCheck (
  Arbitrary (..),
  Gen,
  frequency,
 )

-- | An arbitrary positive integer, mostly small.
arbitraryPositiveInteger :: Gen Integer
arbitraryPositiveInteger =
  arbitraryNonnegativeInteger <&> \case
    0 -> 1
    n -> n

-- | An arbitrary non-negative integer, mostly small.
arbitraryNonnegativeInteger :: Gen Integer
arbitraryNonnegativeInteger =
  frequency
    [ (1, abs <$> arbitrary)
    , (6, floor . sqrt @Double . abs <$> arbitrary)
    ]

-- | An arbitrary integer, mostly small.
arbitraryInteger :: Gen Integer
arbitraryInteger =
  frequency
    [ (1, negate <$> arbitraryPositiveInteger)
    , (6, arbitraryNonnegativeInteger)
    ]

