{-
Colin Gagich
January 28th 2014

made for HackerRank.com Pascal Triangle Challenge
for the functional languages section
-------------------------------------------------
Input N -> Integer - The number of rows to output
Output -> N rows, each on their own line, with each column separated by a space
 -}

row ::  Int -> Int -> [Int]
row i j
	| j == 1 = [1]
	| i == j = (row i (j-1)) ++ [1]
	| otherwise = (row i (j-1)) ++ [(factorial (i-1)) `div` ((factorial (j-1))*(factorial(i-j)))]

gets i = row i i


factorial n 
	| n > 1 = n * factorial (n-1)
	| otherwise = 1

unwrap [] = ""
unwrap xs = show(head xs) ++ " " ++ unwrap (tail xs)

main = do
	n <- readLn :: IO Int
	mapM_ putStrLn [unwrap (gets x) | x <- [1..n]]