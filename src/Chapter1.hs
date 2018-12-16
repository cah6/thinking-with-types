module Chapter1 where

exponentLaw1 :: (b -> a) -> (c -> a) -> Either b c -> a
exponentLaw1 f g (Left b) = f b
exponentLaw1 f g (Right c) = g c

exponentLaw2 :: (Either b c -> a) -> (b -> a, c -> a)
exponentLaw2 f = (\b -> f (Left b), \c -> f (Right c))

exponentDistribute1 :: (c -> (a, b)) -> (c -> a, c -> b)
exponentDistribute1 f = (fst <$> f, snd <$> f)

exponentDistribute2 :: (c -> a, c -> b) -> (c -> (a, b))
exponentDistribute2 (f, g) = \c -> (f c, g c)

exponentMultiply1 :: (c -> (b -> a)) -> ((b, c) -> a)
exponentMultiply1 f = \(b, c) -> (f c) b

exponentMultiply1' :: (c -> b -> a) -> ((b, c) -> a)
exponentMultiply1' = uncurry . flip

exponentMultiply2 :: ((b, c) -> a) -> (c -> (b -> a))
exponentMultiply2 f = \c -> (\b -> f (b, c))