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

-- computes size of a list
computeSize :: [Integer] -> (Integer, [Integer])
computeSize [] = error "Input is empty"
computeSize [x] = (x, [x])
computeSize xs = (sum xs, xs) 

-- computes the sizes of a list of lists
computeSizes :: [[Integer]] -> [(Integer, [Integer])]
computeSizes [] = error "Input is empty"
computeSizes [x] = [computeSize x]
computeSizes (x:xs) = computeSize x : [] ++ computeSizes xs 

-- returns all subsets sorted by their size
sortSubsetsBySize :: [Integer] -> [(Integer, [Integer])]
sortSubsetsBySize = quicksort . computeSizes . generateSubsets

-- gets the k number of smallest subsets of a list
smallestKSets :: Int -> [Integer] -> [(Integer, [Integer])]
smallestKSets k xs = take k (sortSubsetsBySize xs)

-- formatting functions
printHeader = putStr "size\tsublist\n"

formatTuple :: (Integer, [Integer]) -> String
formatTuple (x, xs) = show x ++ "\t" ++ show xs ++ "\n"

formatTuples :: [(Integer, [Integer])] -> String
formatTuples [] = ""
formatTuples xs = formatTuple (head xs) ++ formatTuples (tail xs)

-- main program
main :: Int -> [Integer] -> IO ()
main k xs = do
    printHeader
    let res = smallestKSets k xs
    let res_format = formatTuples res
    putStr (res_format)
