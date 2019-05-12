{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Vector where

-- import GHC.TypeLits (Symbol, Nat)

-- define natural numbers
-- Z = zero, S = Successor, Succ
data Nat = Z | S Nat

-- define a "vector"
data Vector a n where
  Nil  :: Vector a Z
  Cons :: a -> Vector a n -> Vector a (S n)

-- 
emptyVector :: Vector a Z
emptyVector = Nil

oneThing :: Vector Integer (S Z)
oneThing = Cons 1 Nil

twoThings :: Vector Integer (S (S Z))
twoThings = Cons 2 (Cons 1 Nil)

-- emptyVector' :: Vector a Z
-- emptyVector' = Cons 0 Nil

head :: Vector a (S n) -> a
head (Cons x _) = x

reverse :: Vector a (S n) -> Vector a (S n)
reverse = undefined

-- define addition at type level
infixl 6 :+
type family   (n :: Nat) :+ (m :: Nat) :: Nat
type instance Z     :+ m = m
type instance (S n) :+ m = S (n :+ m)

append :: Vector a n -> Vector a m -> Vector a (n :+ m)
append (Cons x xs)  ys = Cons x $ append xs ys
append Nil          ys = ys

zipVec :: Vector a n -> Vector a n -> Vector a n
zipVec = undefined

-- trying to zip length 3 with length 4
test = zipVec (append oneThing twoThings) (append twoThings twoThings)