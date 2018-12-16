{-# LANGUAGE DataKinds, TypeFamilies #-}
module Chapter2 where

import GHC.TypeLits

-- 2.1.3-i: 
-- (Show Int) has kind CONSTRAINT, and Int is a TYPE, so Show must have kind
-- TYPE -> CONSTRAINT

-- 2.1.3-ii: 
-- (Functor a) :: TYPE -> TYPE

-- 2.1.3-iii:
--  Monad requires Applicative, so...
-- (Monad a) :: CONSTRAINT -> TYPE -> TYPE ? 

-- 2.1.3-iv:
-- (MonadTrans a) :: TYPE -> TYPE ? 

-- Term level not...
not :: Bool -> Bool
not True = False
not False = True

type family Not (x :: Bool) :: Bool where
  Not 'True = 'False
  Not 'False = 'True