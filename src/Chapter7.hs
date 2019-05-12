{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE UndecidableInstances      #-}
module Chapter7 where 

import Data.Foldable
import Data.Maybe
import Data.Typeable

data Any where 
  Any :: a -> Any

elimAny :: (forall a. a -> r) -> Any -> r
elimAny f (Any a) = f a

data HasShow where
  HasShow :: (Show a) => a -> HasShow

instance Show HasShow where
  show hs = "HasShow: " ++ elimHasShow show hs

elimHasShow :: (forall a. Show a => a -> r)
  -> HasShow
  -> r
elimHasShow f (HasShow a) = f a

-- 7.1.1
data Dynamic where
  Dynamic :: Typeable t => t -> Dynamic

elimDynamic :: (forall a. Typeable a => a -> r)
  -> Dynamic
  -> r
elimDynamic f (Dynamic a) = f a

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic = elimDynamic cast

liftD2 :: forall a b r. (Typeable a, Typeable b, Typeable r)
  => Dynamic 
  -> Dynamic 
  -> (a -> b -> r)
  -> Maybe Dynamic
liftD2 d1 d2 f = fmap Dynamic . f <$> fromDynamic @a d1 <*> fromDynamic @b d2

pyPlus :: Dynamic -> Dynamic -> Dynamic 
pyPlus a b = fromMaybe (error "bad types for pyPlus") $ asum 
  [ liftD2 @String @String a b (++)
  , liftD2 @Int @Int a b (+)
  , liftD2 @String @Int a b $ \strA intB ->
      strA ++ show intB
  , liftD2 @Int @String a b $ \intA strB ->
      show intA ++ strB
  ]

-- good use: provably local mutation

-- runST :: (forall s. ST s a) -> a
-- ensures s doesn't appear anywhere in return type, i.e. that a doesn't depend on s
