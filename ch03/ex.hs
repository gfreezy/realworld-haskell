import Data.List

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

--mean' :: (Num a, Fractional b) => [a] -> b
mean' [] = 0
mean' xs = sum xs / len
  where len = fromIntegral . length $ xs

palindrome :: [a] -> [a]
palindrome [] = []
palindrome xs = xs ++ reverse' xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome xs = xs == ys
  where ys = reverse' xs

sortByLength :: [[a]] -> [[a]]
sortByLength xs = sortBy f xs
  where f x y = length x `compare` length y


data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

tree = Node 1
       (Node 2 Empty Empty)
       (Node 3
        (Node 4 Empty Empty)
        (Node 5
         (Node 6 Empty Empty)
         Empty))

height :: Tree a -> Int
height Empty = 0
height (Node _ l r) = 1 + maxOfSubtree
  where maxOfSubtree = if hl > hr
                       then hl
                       else hr
        hl = height l
        hr = height r

data Direction = DLeft | DRight | DStraight deriving (Show, Eq)
type PT = Float
type Point = (PT, PT)

points :: [Point]
points = [(-3,1),(-4,1),(-1,4),(0,0),(2,2),(-1,3),(-1,2),(1,0),(3,-1),(-1,-1)]

cross :: Point -> Point -> Point -> PT
cross (x1, y1) (x2, y2) (x3, y3) = (x2-x1)*(y3-y1) - (y2-y1)*(x3-x1)

dist :: Point -> Point -> PT
dist (x1, y1) (x2, y2) = sqrt((x1-x2)^2 + (y1-y2)^2)

turn :: Point -> Point -> Point -> Direction
turn p1 p2 p3
  | r == 0 = DStraight
  | r < 0 = DRight
  | r > 0 = DLeft
  where r = cross p1 p2 p3

compareYPoint :: Point -> Point -> Ordering
compareYPoint (x1, y1) (x2, y2) = if y1 == y2
                                  then x1 `compare` x2
                                  else y1 `compare` y2

lowestY :: [Point] -> Point
lowestY = minimumBy compareYPoint

compareAngle :: Point -> Point -> Point -> Ordering
compareAngle p p1 p2 = computeCosine p p1 `compare` computeCosine p p2

computeCosine :: Point -> Point -> PT
computeCosine p1@(x1, y1) p2@(x2, y2) = negate $ (x2-x1) / dist p1 p2

sortPoints :: [Point] -> [Point]
sortPoints points = pvt : sortBy (compareAngle pvt) (delete pvt p)
  where p = nub points
        pvt = lowestY p

gScan :: [Point] -> [Point]
gScan = foldl f [] . sortPoints
  where f (x:y:acc) p = case turn y x p of
          DLeft -> p:x:y:acc
          DStraight -> p:x:y:acc
          DRight -> f (y:acc) p
        f acc p = p:acc
