import Data.Char (digitToInt, isDigit, isSpace)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (x:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit [x] = Just []
safeInit (x:xs) = let Just ys = safeInit xs
                  in Just $ x:ys

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f xs = let (pre, suf) = span f xs
                 in pre : case suf of
                   [] -> []
                   (x:xs') -> splitWith f xs'

firstWordOfLine :: String -> String
firstWordOfLine = head . words

type ErrorMessage = String

asInt_fold :: String -> Either ErrorMessage Int
asInt_fold [] = Left "Empty String"
asInt_fold ['-'] = Left "Minus only"
asInt_fold ('-':_) = Left "Negative Number"
asInt_fold xs = foldl step (Right 0) xs
  where step (Right acc) x = if isDigit x then
                               let r = acc * 10 + (digitToInt x)
                               in if r < acc then
                                    Left "Overflow"
                                  else
                                    Right r
                             else
                               Left $ "Not a digit '" ++ show x ++ "'"
        step acc _ = acc

concat' :: [[a]] -> [a]
concat' = foldr (++) []

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
  | f x = x : takeWhile' f xs
  | otherwise = []

takeWhileFold :: (a -> Bool) -> [a] -> [a]
takeWhileFold f = foldr step []
  where step x acc
          | f x = x:acc
          | otherwise = []

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _ [] = []
groupBy' f xs = reverse . snd $ foldl step (head xs,[[head xs]]) (tail xs)
  where step (x, (a:acc)) y = if f x y then
                               (x, (y:a):acc)
                             else
                               (y, [y]:a:acc)

any' :: (a -> Bool) -> [a] -> Bool
any' f = foldr ((||).f) False

words' :: String -> [String]
words' [] = []
words' xs =  foldr step [[]] xs
  where step x (a:acc)
          | isSpace x = if null a then []:acc else []:a:acc
          | otherwise =  (x:a):acc

unlines' :: [String] -> String
unlines' = foldr step []
  where step x acc = x++'\n':acc
