{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module Chapter10 where
import Data.Kind (Constraint, Type)
import           Data.Maybe
import           Prelude hiding (fst)

class Eval l t | l -> t where
  eval :: l -> t

-- fst things
data Fst a b = Fst (a, b)

instance Eval (Fst a b) a where
  eval (Fst (a, b)) = a

-- fromMaybe things
data ListToMaybe a = ListToMaybe [a]

instance Eval (ListToMaybe a) (Maybe a) where
  eval (ListToMaybe xs) = listToMaybe xs

-- MapList things
data MapList dfb a = MapList (a -> dfb) [a]

instance Eval dfb dft => Eval (MapList dfb a) [dft] where
  eval (MapList f []) = []
  eval (MapList f (a:as)) = eval (f a):eval (MapList f as)