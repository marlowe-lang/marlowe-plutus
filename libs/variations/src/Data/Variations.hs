{--
{-# LANGUAGE AllowAmbiguousTypes #-}
--}

module Data.Variations where

import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import Data.Data (Proxy)
import Data.Fixed (Fixed, HasResolution)
import Data.Foldable (fold)
import Data.Int
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Internal as Map
import qualified Data.Set.Internal as Set
import Data.Set.NonEmpty.Internal
import Data.Text (Text)
import Data.Time (Day, DiffTime, NominalDiffTime, UTCTime (..), secondsToDiffTime, secondsToNominalDiffTime)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Version (Version)
import Data.Word
import GHC.Generics
import GHC.Real (Ratio ((:%)))
import Numeric.Natural (Natural)

-- | A class for generating variations of a data type. morally, at least one variation of
-- each constructor should be generated.
--
-- Warning: do not use the generic implementation for recursive types - it
-- will generate an infinite list.
class Variations a where
  variations :: NonEmpty a
  default variations :: (Generic a, GVariations (Rep a)) => NonEmpty a
  variations = to <$> gVariations

instance Variations Version

instance (Variations a) => Variations [a] where
  variations = [] :| NE.toList (pure <$> variations)

instance (Variations a) => Variations (Set.Set a) where
  variations = Set.Tip :| NE.toList (Set.Bin 1 <$> variations <*> pure Set.Tip <*> pure Set.Tip)

instance (Variations k, Variations a) => Variations (Map.Map k a) where
  variations = Map.Tip :| NE.toList (Map.Bin 1 <$> variations `varyAp` variations <*> pure Map.Tip <*> pure Map.Tip)

instance (Variations a, Ord a) => Variations (NESet a) where
  variations = case NE.sort variations of
    a :| as -> NESet <$> (a :| as) `varyAp` (Set.Tip :| (Set.Bin 1 <$> as <*> pure Set.Tip <*> pure Set.Tip))

instance Variations UTCTime where
  variations = UTCTime <$> variations `varyAp` variations

instance Variations DiffTime where
  variations = secondsToDiffTime <$> variations

instance Variations NominalDiffTime where
  variations = secondsToNominalDiffTime <$> variations

instance (HasResolution a) => Variations (Fixed a) where
  variations = pure 1

instance Variations Day where
  variations = pure $ fromOrdinalDate 2000 1

instance (Variations a, Variations b) => Variations (a, b)

instance (Variations a) => Variations (Maybe a)

instance (Variations a, Variations b) => Variations (Either a b)

instance Variations Bool

instance Variations (Proxy a)

instance Variations ()

instance Variations Word8 where
  variations = pure 1

instance Variations Word16 where
  variations = pure 1

instance Variations Word32 where
  variations = pure 1

instance Variations Word64 where
  variations = pure 1

instance Variations Int8 where
  variations = pure 1

instance Variations Int16 where
  variations = pure 1

instance Variations Int32 where
  variations = pure 1

instance Variations Int64 where
  variations = pure 1

instance Variations Float where
  variations = pure 1

instance Variations Double where
  variations = pure 1

instance Variations Int where
  variations = pure 1

instance (Variations a) => Variations (Ratio a) where
  variations = (:%) <$> variations `varyAp` variations

instance Variations Char where
  variations = pure 'a'

instance Variations Natural where
  variations = pure 1

instance Variations Integer where
  variations = pure 1

instance Variations ByteString where
  variations = mempty :| ["a"]

instance Variations BS.ByteString where
  variations = mempty :| ["a"]

instance Variations Text where
  variations = mempty :| ["a"]

class GVariations f where
  gVariations :: NonEmpty (f a)

instance GVariations U1 where
  gVariations = pure U1

instance (GVariations f, GVariations g) => GVariations (f :+: g) where
  gVariations = (L1 <$> gVariations) <> (R1 <$> gVariations)

instance (GVariations f, GVariations g) => GVariations (f :*: g) where
  gVariations = (:*:) <$> gVariations `varyAp` gVariations

instance (Variations c) => GVariations (K1 i c) where
  gVariations = K1 <$> variations

instance (GVariations f) => GVariations (M1 i t f) where
  gVariations = M1 <$> gVariations

-- | A combinator that can be used like <*> except it doesn't take the
-- cartesian product. Instead, given a list of N elements and a list of M elements,
-- it produces a list of (1 - N + M) elements by applying the head of the first
-- list to the head of the second, then applying the head of the first list to
-- the tail of the second, and applying the tail of the second list to the head
-- of the first. Useful for defining `variations`.
varyAp :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
varyAp (f :| fs) (a :| as) =
  f a
    :| fold
      [ f <$> as
      , ($ a) <$> fs
      ]

infixl 4 `varyAp`

-- | A version of varyAp for optional arguments.
varyAp' :: NonEmpty (Maybe a -> b) -> [a] -> NonEmpty b
varyAp' (f :| fs) as =
  f Nothing
    :| fold
      [ f . Just <$> as
      , case as of
          a : _ -> ($ Just a) <$> fs
          _ -> []
      ]

infixl 4 `varyAp'`
