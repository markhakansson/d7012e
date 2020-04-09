-- sorting
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

-- create subsets starting from the last element in the list
subsetsLastElement :: [Integer] -> [[Integer]] 
subsetsLastElement [] = error "Input is empty!"
subsetsLastElement [x] = [[x]]
subsetsLastElement xs = [xs] ++ subsetsLastElement (tail xs) 

-- generate all possible subsets of a list 
generateSubsets :: [Integer] -> [[Integer]]
generateSubsets [] = error "Input is empty!"
generateSubsets [x] = [[x]]
generateSubsets xs = subsetsLastElement xs ++ generateSubsets (init xs)


