import Data.Char (toUpper)

square :: [Double] -> [Double]
square [] = []
square (x:xs) = x*x : square xs

upperCase :: String -> String
upperCase [] = []
upperCase (x:xs) = toUpper x : upperCase xs

square2 xs = map squareOne xs
  where squareOne x = x*x

upperCase2 xs = map toUpper xs

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs
