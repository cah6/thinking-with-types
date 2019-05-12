{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter8 where

import           Data.Coerce (Coercible(..), coerce)
import           Data.Foldable (toList)
import qualified Data.Map as M
import           Data.Monoid (Sum(..), Product(..))

-- prefer coerce
fastSum :: [Int] -> Int
fastSum = getSum . mconcat . coerce

-- over (fmap Newtype)
slowSum :: [Int] -> Int
slowSum = getSum . mconcat . fmap Sum

type family IntToBool a where
  IntToBool Int = Bool
  IntToBool a = a

data BST v = Empty
           | Branch (BST v) v (BST v)

type role BST nominal