{-# LANGUAGE NoImplicitPrelude #-}

-- | A dictionary (map) with keys hashed to minimize
-- | storage requirements on-chain.
module Language.Marlowe.Contrib.PlutusTx.KeyHashedDict
  ( KeyHashedDict
  , KeyHash
  , delete
  , empty
  , insert
  , insertWith
  , lookup
  , keyHashes
  , member
  , mkKeyHash
  , toList
  , unionWith
  , unsafeMkKeyHash
  , values
  )
  where

import PlutusTx.Prelude
import PlutusTx.Foldable (Foldable(foldr))
import PlutusTx.Traversable (Traversable(traverse))

import Language.Marlowe.Contrib.PlutusTx.Dict (Dict)
import qualified Language.Marlowe.Contrib.PlutusTx.Dict as Dict

newtype KeyHashedDict v = KeyHashedDict (Dict v)

instance Eq v => Eq (KeyHashedDict v) where
  {-# INLINEABLE (==) #-}
  (KeyHashedDict d1) == (KeyHashedDict d2) = d1 == d2

instance Functor KeyHashedDict where
  {-# INLINEABLE fmap #-}
  fmap f (KeyHashedDict d) = KeyHashedDict (fmap f d)

instance Foldable KeyHashedDict where
  {-# INLINEABLE foldr #-}
  foldr f z (KeyHashedDict d) = foldr f z d

instance Traversable KeyHashedDict where
  {-# INLINEABLE traverse #-}
  traverse f (KeyHashedDict d) = KeyHashedDict <$> traverse f d

instance Semigroup v => Semigroup (KeyHashedDict v) where
  {-# INLINEABLE (<>) #-}
  (KeyHashedDict d1) <> (KeyHashedDict d2) = KeyHashedDict (d1 <> d2)

instance Monoid v => Monoid (KeyHashedDict v) where
  {-# INLINEABLE mempty #-}
  mempty = KeyHashedDict mempty

newtype KeyHash = KeyHash BuiltinByteString

{-# INLINEABLE delete #-}
delete :: KeyHash -> KeyHashedDict v -> KeyHashedDict v
delete (KeyHash h) (KeyHashedDict d) = KeyHashedDict (Dict.delete h d)

{-# INLINEABLE empty #-}
empty :: KeyHashedDict v
empty = KeyHashedDict Dict.empty

{-# INLINEABLE insert #-}
insert :: KeyHash -> v -> KeyHashedDict v -> KeyHashedDict v
insert (KeyHash h) v (KeyHashedDict d) = KeyHashedDict (Dict.insert h v d)

{-# INLINEABLE insertWith #-}
insertWith :: KeyHash -> v -> (v -> v -> v) -> KeyHashedDict v -> KeyHashedDict v
insertWith (KeyHash h) v f (KeyHashedDict d) = KeyHashedDict (Dict.insertWith h v f d)

{-# INLINEABLE keyHashes #-}
keyHashes :: KeyHashedDict v -> [KeyHash]
keyHashes (KeyHashedDict d) = fmap KeyHash (Dict.keys d)

{-# INLINEABLE lookup #-}
lookup :: KeyHash -> KeyHashedDict v -> Maybe v
lookup (KeyHash h) (KeyHashedDict d) = Dict.lookup h d

{-# INLINEABLE member #-}
member :: KeyHash -> KeyHashedDict v -> Bool
member (KeyHash h) (KeyHashedDict d) = Dict.member h d

{-# INLINEABLE mkKeyHash #-}
mkKeyHash :: BuiltinByteString -> KeyHash
mkKeyHash = KeyHash. takeByteString 20 . sha2_256

{- HLINT ignore "Use first" -}
{-# INLINEABLE toList #-}
toList :: KeyHashedDict v -> [(KeyHash, v)]
toList (KeyHashedDict d) = fmap (\(k, v) -> (KeyHash k, v)) (Dict.toList d)

{-# INLINEABLE unionWith #-}
unionWith :: (v -> v -> v) -> KeyHashedDict v -> KeyHashedDict v -> KeyHashedDict v
unionWith f (KeyHashedDict d1) (KeyHashedDict d2) = KeyHashedDict (Dict.unionWith f d1 d2)

{-# INLINEABLE unsafeMkKeyHash #-}
unsafeMkKeyHash :: BuiltinByteString -> KeyHash
unsafeMkKeyHash = KeyHash

{-# INLINEABLE values #-}
values :: KeyHashedDict v -> [v]
values (KeyHashedDict d) = Dict.values d

