{-# LANGUAGE TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes #-}
module Chapter4 where

import Data.Typeable

typeName :: forall a. Typeable a => String
typeName = show . typeRep $ Proxy @a
