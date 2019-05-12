{-# LANGUAGE KindSignatures, DataKinds, GeneralizedNewtypeDeriving #-}

module Money where

import GHC.TypeLits (Symbol, Nat)
import Data.Ratio ((%))

newtype Money (currency :: Symbol) = Money Rational 
  deriving Num

add :: Money c -> Money c -> Money c
add (Money x) (Money y) = Money (x + y)

-- type USD = "USD"
-- type EUR = "EUR"

fiveDollars :: Money "USD"
fiveDollars = Money 5

tenDollars :: Money "USD"
tenDollars = Money 10

twoEuros :: Money "EUR"
twoEuros = Money 2

addingWorks :: Money "USD"
addingWorks = fiveDollars + tenDollars

addingFails = fiveDollars + twoEuros

----------

newtype Discrete (currency :: Symbol) (scale :: (Nat, Nat)) = Discrete Integer

oneDollar :: Discrete "USD" '(1, 1)
oneDollar = Discrete 1

oneDollarThirtyCents :: Discrete "USD" '(100, 1)
oneDollarThirtyCents = Discrete 130