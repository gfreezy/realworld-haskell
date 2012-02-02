-- file: ch01/WC.hs
-- lines beginning with "--" are comments.

main = interact wordCount
  where wordCount input = show (length input) ++ "\n"

last' :: [a] -> a
last' (x:y:[]) = x
last' (_:xs) = last' xs
