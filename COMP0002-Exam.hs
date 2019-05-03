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

signNum :: Int -> Int
signNum x = if x < 0 then -1 else
			if x == 0 then 0 else 
			1

signNumGuard :: Int -> Int
signNumGuard x	| x < 0		= -1
				| x == 0	= 0
				| otherwise = 1
				
safeTail :: [ x ] -> [ x ]
safeTail [ ] = [ ]
safeTail (x : xs) = xs

factorial :: Int -> Int
factorial 0 = 1
factorial x = (factorial (x - 1)) * x

qsort :: (Ord a) => [ a ] -> [ a ]
qsort [ ] = [ ] 
qsort (x : xs) = qsort [a | a <- xs, a <= x] 
					++ [ x ] ++ 
					qsort [b | b <- xs, b > x]

insert :: (Ord a) => a -> [ a ] -> [ a ] 
insert x [ ] = [x]
insert x (y : ys) = if (x > y)	then [ y ] ++ (insert x ys)
								else [ x ] ++ [ y ] ++ ys
