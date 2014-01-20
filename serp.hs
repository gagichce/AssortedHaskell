--triangle shape
tri = ["1","111"]
serpy n = mapM_ putStrLn (center (build n))
build :: Int -> [String]
build 1 = tri
build n = (build (n-1)) ++ inverseFill (build (n-1))

inverseFill xs = specialConcat(zip xs(reverse [ul (length x) | x <- xs]))
specialConcat xs = [a++b++a | (a,b) <- xs]
center xs = [ surround x (maxLength xs) | x <- xs]
surround x n =  (ul (fromIntegral ((n-length x) `div` 2))) ++ x ++ (ul (fromIntegral ((n-length x) `div` 2)))

maxLength :: [String] -> Int
maxLength xs = maximum ([length x | x <- xs])
ul n = take n (repeat '_')