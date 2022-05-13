{-# LANGUAGE DeriveTraversable #-}

module Ariel.Runtime.Env where

data Env a
  = EmptyEnv
  | ConsEnv a (Env a)
  deriving (Eq, Show, Foldable, Functor, Traversable)

instance Semigroup (Env a) where
  (<>) = appendEnv

instance Monoid (Env a) where
  mempty = emptyEnv

emptyEnv :: Env a
emptyEnv = EmptyEnv
{-# INLINE emptyEnv #-}

extendEnv :: a -> Env a -> Env a
extendEnv = ConsEnv
{-# INLINE extendEnv #-}

lookupEnv :: Int -> Env a -> Maybe a
lookupEnv _ EmptyEnv = Nothing
lookupEnv 0 (ConsEnv x _) = Just x
lookupEnv n (ConsEnv _ xs) = lookupEnv (n - 1) xs

appendEnv :: Env a -> Env a -> Env a
appendEnv EmptyEnv ys = ys
appendEnv (ConsEnv x xs) ys = ConsEnv x (appendEnv xs ys)

fromList :: [a] -> Env a
fromList = foldr ConsEnv EmptyEnv
