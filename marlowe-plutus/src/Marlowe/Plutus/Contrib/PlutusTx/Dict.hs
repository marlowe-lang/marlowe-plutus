{-# LANGUAGE NoImplicitPrelude #-}

module Marlowe.Plutus.Contrib.PlutusTx.Dict
  ( Dict
  , empty
  , insert
  , insertWith
  , lookup
  , delete
  , member
  , keys
  , values
  , toList
  , fromList
  , unionWith
  )
  where

import PlutusTx.Prelude
import PlutusTx.List as List
import PlutusTx.Foldable (Foldable(foldr))
import PlutusTx.Traversable (Traversable(traverse))

-- | Internally Dict is a sorted list of key-value pairs.
-- | This makes merging two Dicts efficient so folding
-- | over a list of Dicts is efficient as well.
newtype Dict v = Dict [(BuiltinByteString, v)]

instance Eq v => Eq (Dict v) where
  {-# INLINEABLE (==) #-}
  (Dict xs) == (Dict ys) = xs == ys

{- HLINT ignore "Use second" -}
instance Functor Dict where
  {-# INLINEABLE fmap #-}
  fmap f (Dict xs) = Dict (fmap (\(k, v) -> (k, f v)) xs)

instance Foldable Dict where
  {-# INLINEABLE foldr #-}
  foldr f z (Dict xs) = go xs z
    where
      go [] acc = acc
      go ((_, v) : xs') acc = f v (go xs' acc)

instance Traversable Dict where
  {-# INLINEABLE traverse #-}
  traverse f (Dict dict) = Dict <$> go dict
    where
      go [] = pure []
      go ((k, v) : xs) = do
        let
          x' = (k,) <$> f v
          xs' = go xs
        (:) <$> x' <*> xs'

instance Semigroup v => Semigroup (Dict v) where
  {-# INLINEABLE (<>) #-}
  dict1 <> dict2 = unionWith (<>) dict1 dict2

instance Monoid v => Monoid (Dict v) where
  {-# INLINEABLE mempty #-}
  mempty = empty

{-# INLINEABLE empty #-}
empty :: Dict v
empty = Dict []

{-# INLINEABLE insert #-}
insert :: BuiltinByteString -> v -> Dict v -> Dict v
insert k v (Dict dict) = Dict (go dict)
  where
    go [] = [(k, v)]
    go xs@((k', v') : xs')
      | k > k' = (k', v') : go xs'
      | k < k' = (k, v) : xs
      | otherwise = (k, v) : xs'

{-# INLINEABLE insertWith #-}
-- | Insert with a combining function if the key already exists
-- | The combining function takes the new value first and the old value second.
insertWith :: BuiltinByteString -> v -> (v -> v -> v) -> Dict v -> Dict v
insertWith k v f (Dict dict) = Dict (go dict)
  where
    go [] = [(k, v)]
    go xs@((k', v') : xs')
      | k > k' = (k', v') : go xs'
      | k < k' = (k, v) : xs
      | otherwise = (k, f v v') : xs'

{-# INLINEABLE lookup #-}
lookup :: BuiltinByteString -> Dict v -> Maybe v
lookup k (Dict xs) = go xs
  where
    go [] = Nothing
    go ((k', v') : xs')
      | k > k' = go xs'
      | k < k' = Nothing
      | otherwise = Just v'

{-# INLINEABLE delete #-}
delete :: BuiltinByteString -> Dict v -> Dict v
delete k (Dict dict) = Dict (go dict)
  where
    go [] = []
    go xs@((k', v') : xs')
      | k > k' = (k', v') : go xs'
      | k < k' = xs
      | otherwise = xs'

{-# INLINEABLE member #-}
member :: BuiltinByteString -> Dict v -> Bool
member k dict = isJust (lookup k dict)

{-# INLINEABLE keys #-}
keys :: Dict v -> [BuiltinByteString]
keys (Dict xs) = fmap fst xs

{-# INLINEABLE values #-}
values :: Dict v -> [v]
values (Dict xs) = fmap snd xs

{-# INLINEABLE toList #-}
toList :: Dict v -> [(BuiltinByteString, v)]
toList (Dict xs) = xs

{-# INLINEABLE fromList #-}
fromList :: [(BuiltinByteString, v)] -> Dict v
fromList dict = Dict (go (List.sortBy (\(k1, _) (k2, _) -> compare k1 k2) dict))
  where
    go [] = []
    go [x] = [x]
    go ((k1, v1) : xs@((k2, _) : xs'))
      | k1 == k2 = go ((k2, v1) : xs')
      | otherwise = (k1, v1) : go xs

{-# INLINEABLE unionWith #-}
unionWith :: (v -> v -> v) -> Dict v -> Dict v -> Dict v
unionWith f (Dict dict1) (Dict dict2) = fromList (go dict1 dict2)
  where
    go [] ys = ys
    go xs [] = xs
    go xs@((k1, v1) : xs') ys@((k2, v2) : ys')
      | k1 < k2 = (k1, v1) : go xs' ys
      | k1 > k2 = (k2, v2) : go xs ys'
      | otherwise = (k1, f v1 v2) : go xs' ys'


