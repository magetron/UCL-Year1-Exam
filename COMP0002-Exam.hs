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

myMap :: (a -> b) -> [ a ] -> [ b ]
myMap f [ ] = [ ]
myMap f (x : xs) = [f x] ++ (map f xs)

myMapHigher :: (a -> b) -> [ a ] -> [ b ]
myMapHigher f xs = [f x | x <- xs]

myFilter :: (a -> Bool) -> [ a ] -> [ a ]
myFilter f [ ] = [ ]
myFilter f (x : xs) 	| f x == True	= x : (myFilter f xs)
						| otherwise 	= myFilter f xs

data Answer = Yes | No | Unknown

answers :: [Answer]
answers = [Yes, No, Unknown]

flipAnswers :: Answer -> Answer
flipAnswers Yes = No
flipAnswers No = Yes
flipAnswers Unknown = Unknown

squareSum :: [ Int ] -> Int
squareSum xs = foldr (+) 0 (map sqr xs)
				where	sqr x = x * x

largestDivisible :: (Integral a) => a
largestDivisible = head (filter divise [1000, 999 ..])
					where divise x = mod x 7 == 0

sq :: Int -> Int
sq x = x * x

comp2 :: (a -> b) -> (b -> b -> c) -> (a -> a -> c) 
comp2 f g = (\x y -> g (f x) (f y)) 

total :: (Integer -> Integer) -> (Integer -> Integer)
total f n = foldr (+) 0 (map f [0..n])

myFlip :: (a -> b -> c) -> (b -> a -> c)
myFlip f a b = f b a

doSomething :: [ Int ] -> [ Int ]
doSomething xs = (filter (>0) . map (+1)) xs

doSomething2 :: [ Int ] -> [ Int ]
doSomething2 xs = (map (+1) . filter (>(-1))) xs

curry3 :: ((a, b, c) -> d) -> (a -> b -> c -> d)
curry3 f a b c = f (a, b, c)

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (x, y, z) = f x y z

data Expr	= Val Int
			| Add Expr Expr
			| Mul Expr Expr

size :: Expr -> Int
size (Val n)	= 1
size (Add x y)	= size x + size y
size (Mul x y)	= size x + size y

eval :: Expr -> Int
eval (Val n)	= n
eval (Add x y)  = eval x + eval y
eval (Mul x y)	= eval x * eval y

myGetLine :: IO()
myGetLine = do 
				x <- getLine
				putStrLn x
				
myPrinter :: Int -> IO()
myPrinter x = print x
