{-# LANGUAGE InstanceSigs #-}
module Chapter3 where

newtype T1 a = T1 (Int -> a)

instance Functor T1 where
  fmap :: (a -> b) -> T1 a -> T1 b
  fmap f (T1 g) = T1 $ f . g
  -- need :: b
  -- f :: a -> b
  -- g :: (Int -> a)
  -- i :: Int

newtype T5 a = T5 ((a -> Int) -> Int)

instance Functor T5 where
  fmap :: (a -> b) -> T5 a -> T5 b
  fmap f (T5 g) = T5 $ \u -> g (u . f) -- no idea. how can you apply a lambda backwards twice?
  -- u :: a -> Int
  -- actually u has to be (b -> Int)
  -- f :: a -> b
  -- g :: (a -> Int) -> Int
  -- but I need :: Int

u :: String -> Int
u = length

f :: String -> Bool
f = null

type Ts = T5 String

type Tb = T5 Bool

changeIt :: Ts -> Tb
changeIt ts = fmap null ts

-- f :: String -> Int, like length
exTs :: Ts
exTs = T5 $ \f -> f "yello"
