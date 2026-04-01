module Marlowe.Testing.Contrib.Data.Functor where

flap :: Functor f => f (a -> b) -> a -> f b
flap ff x = (\f -> f x) <$> ff
{-# INLINE flap #-}

infixl 4 <@>
(<@>) :: Functor f => f (a -> b) -> a -> f b
(<@>) = flap
{-# INLINE (<@>) #-}
