--frac n = fill n 1

--fill n = 

--takes a string and makes sure it has the right number of __ around it
surround st = (spaces (floor (remainFill st))) ++ (st ++ (spaces (ceiling (remainFill st))))
--calculates the "right" number of __ to be around something
remainFill  st = (100.0 -(fromIntegral (length st)))/2.0
--makes 1__n__1 pattern
oneSpace n = if n <= 1 then "1" else ('1' : ((spaces n) ++ ['1']))
--makes 1__n__ pattern
oneSpaceNoOne n = (['1'] ++ (spaces n))
noChange :: Int -> String
noChange n = surround ((take (((nSpaceBlank n) + 1)* (2^(n-1) - 1)) (cycle (oneSpaceNoOne (nSpaceBlank n)))) ++ ['1'])

nSpaceBlank n = if n <= 1 then 0 else (truncate 2^(7-n)) - 1

--returns n number of '_'
spaces n = replicate n '_'