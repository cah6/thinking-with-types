{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
module Chapter6 where

newtype Cont a = Cont
  { unCont :: forall r. (a -> r) -> r
  }

instance Functor Cont where
  fmap :: (a -> b) -> Cont a -> Cont b
  fmap f (Cont unCont) = Cont $ \a -> unCont (a . f)

instance Applicative Cont where
  pure :: a -> Cont a
  pure a = Cont $ \f -> f a
  (<*>) :: Cont (a -> b) -> Cont a -> Cont b
  Cont f <*> Cont a = Cont $ \g -> f (g . a)

instance Monad Cont where
  (>>=) :: Cont a -> (a -> Cont b) -> Cont b
  Cont a >>= f = Cont $ \g -> a (\z -> unCont (f z) g)

applyToFive :: (forall a. a -> a) -> Int
applyToFive f = f 5

-- rank as "depth" of polymorphism
foo :: forall r. (forall a. a -> r) -> r
foo f = f "hello"

runFoo = foo (\a -> 10)