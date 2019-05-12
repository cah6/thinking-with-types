{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Test where

-- import Sorted
-- import The
-- import Named
import GDP
import Theory.Named (type (~~))

newtype Sorted a = Sorted a
sorted = Sorted [1, 2, 3]

newtype SortedBy comp a = SortedBy a
sortedBy = SortedBy [1, 2, 3]

-- sortBy :: ((a -> a -> Ordering) ~~􏰅 comp)
--   -> [a]
--   -> SortedBy comp [a]
-- sortBy comp xs = coerce (L.sortBy (the comp) xs)

-- mergeBy :: ((a -> a -> Ordering) ~ comp) 
--   -> SortedBy comp [a]
--   -> SortedBy comp [a]
--   -> SortedBy comp [a] 
-- mergeBy comp xs ys = undefined


-- -- "name" takes in a thing, and returns it with a temporary name
-- main = name (comparing Down) $ \gt -> 