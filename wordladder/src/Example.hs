module Example where 
    
{- 
    takeFirst $ filter (> 10000000) [1..]
-}

takeFirst :: [Integer] -> Integer
takeFirst [] = undefined
takeFirst (x : _xs) = x