module ArrayMIRIANA where

type Array = [Int]


-- This function has the purpose to declare an Array variable. Given in input the dimension dim that the array should have, 
--i return an Array variable
arrayDeclaration ::  Int -> Array
arrayDeclaration 0 = []
arrayDeclaration dim = 0 : arrayDeclaration (dim-1)  


-- This function has the purpose to read a certain element from an Array, give in input the array itself and the index of the position to read
-- It returns the int contained in position i (considering that index starts from 0)

readElemArray :: Array -> Int -> Int
readElemArray [] _ = error "Empty Array! Index out of bounds!"
readElemArray arr i = arr !! i 


-- This function has the purpose to insert an int element in the array, in a certain position.
-- If the index is 0, the element is inserted in first position, otherwise if it's negative it returns an error.
-- If it's bigger than 0, the function is applied recursively

insertElemArray :: Array -> Int -> Int -> Array
insertElemArray (a:arr) 0 el = el : arr
insertElemArray (a:arr) i el | i < 0 = error "negative index"
                             | otherwise = a : insertElemArray arr (i-1) el       


-- Creates an array full of zeros of the given size
createEmptyArray :: Int -> [Int]
createEmptyArray size = if (size < 1)
    then error "Size error"
    else
        if (size == 1)
            then [0]
            else [0] ++ (createEmptyArray (size -1))


-- Given a position, removes from the list the element in that position
removeElem :: [a] -> Int -> [a]
removeElem [] _ = error "Index out of bound"
removeElem (x : xs) pos = if (pos < 0)
    then error "Negative index"
    else
        if (pos == 0)
            then xs
            else ([x] ++ removeElem xs (pos - 1))