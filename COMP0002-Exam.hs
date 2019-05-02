myMaximum :: [ Int ] -> Int
myMaximum [ ] = -32767
myMaximum (x : xs) = if (myMaximum(xs) > x) then myMaximum(xs) 
											 else x
myLength :: [ x ] -> Int
myLength [ ] = 0
myLength (x : xs) = 1 + myLength(xs)

add :: Int -> Int -> Int
add x y = x + y

absoluteVal :: Int -> Int
absoluteVal x   | x >= 0    = x
                | otherwise = -x

equalTo :: (Eq a) => a -> a -> Bool
equalTo x y = x == y
