myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f zero xs = foldr step id xs zero
  where step x g a = g (f a x)
