{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Chapter5 where

import Data.Kind (Constraint, Type)

data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)
infixr 5 :#

type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
  All c '[] = () 
  All c (t ': ts) = (c t, All c ts) 

hLength :: HList ts -> Int
hLength HNil = 0
hLength (_ :# ts) = 1 + hLength ts

-- instance Eq (HList '[]) where
--   HNil == HNil = True

-- instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
--   (a :# as) == (b :# bs) = a == b && as == bs

instance All Eq ts => Eq (HList ts) where
  HNil == HNil = True
  (a :# as) == (b :# bs) = a == b && as == bs

-- instance Ord (HList '[]) where
--   compare HNil HNil = EQ

-- instance (Ord t, Ord (HList ts)) => Ord (HList (t ': ts)) where
--   compare (a :# as) (b :# bs) = case compare a b of
--     EQ -> compare as bs
--     GT -> GT
--     LT -> LT

instance (All Eq ts, All Ord ts) => Ord (HList ts) where
  compare HNil HNil = EQ
  compare (a :# as) (b :# bs) = case compare a b of
    EQ -> compare as bs
    GT -> GT
    LT -> LT  

-- instance Show (HList '[]) where
--   show HNil = "Nil"

-- instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where
--   show (a :# as) = show a <> ", " <> show as

instance All Show ts => Show (HList ts) where
  show HNil = "Nil"
  show (a :# as) = show a <> ", " <> show as