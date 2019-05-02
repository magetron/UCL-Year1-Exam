myMaximum :: [ Int ] -> Int
myMaximum [ ] = -32767
myMaximum (x : xs) = if (myMaximum(xs) > x) then myMaximum(xs) 
											 else x;
myLength :: [ x ] -> Int
myLength [ ] = 0
myLength (x : xs) = 1 + myLength(xs);


