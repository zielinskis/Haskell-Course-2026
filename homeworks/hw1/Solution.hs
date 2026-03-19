{-# LANGUAGE BangPatterns #-}
module Hw1 (main) where

--exercise 3
sieve :: [Int] -> [Int] --assume that the list of ints we get is [2..n]
sieve [] = []
sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0]

primesTo :: Int -> [Int]
primesTo n = sieve [2..n]

isPrime :: Int -> Bool
isPrime n = elem n (primesTo n)

--exercise 1
goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n = [(p,q) | p <- [2..n`div`2], q <- [p..n] , isPrime p, isPrime q, p+q == n] --p up to n div 2 since q is at least p and p+q=n

--exercise 2
coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs a = [(x,y) | x <- a, y <- a, x < y, gcd x y == 1]

--exercise 4
matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul a b =  [[sum [ a !! i !! k * b !! k !! j | k <- [0 .. p-1] ] | j <- [0..n-1]] | i <- [0..m-1]]
  where
    p = length b
    m = length a
    n = length (b !! 0)

--exercise 5
permutations :: Int -> [a] -> [[a]]
permutations 0 _ = [[]]
permutations k list = concat [ map ((list !! i) :) (permutations (k - 1) [list !! j | j <- [0..length list - 1], j /= i]) | i <- [0 .. length list - 1] ]

--exercise 6
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = smaller x y
  where
    smaller x y
      | x < y = x : merge xs (y:ys)
      | y < x = y : merge (x:xs) ys
      | y == x = x : merge xs ys


hamming :: [Integer]
hamming = 1 : merge (merge (map (*2) hamming) (map (*3) hamming )) (map (*5) hamming)
  
--exercise 7
power_helper :: Int -> Int -> Int -> Int
power_helper b 0 acc = acc
power_helper b e !acc =  (power_helper b (e-1) (acc*b))

power :: Int -> Int -> Int
power b e = power_helper b e 1

--exercise 8
listMax_helper_bang :: [Int] -> Int -> Int
listMax_helper_bang [] !acc = acc
listMax_helper_bang (x:xs) !acc = listMax_helper_bang xs (bigger x acc)
  where
    bigger x acc
      | x > acc = x
      | otherwise = acc

listMax_helper_seq :: [Int] -> Int -> Int
listMax_helper_seq [] acc = acc
listMax_helper_seq (x:xs) acc = listMax_helper_seq xs (seq acc (bigger x acc))
  where
      bigger x acc
        | x > acc = x
        | otherwise = acc


listMax_bang :: [Int] -> Int
listMax_bang (x:xs) = listMax_helper_bang xs x


listMax_seq :: [Int] -> Int
listMax_seq (x:xs) = listMax_helper_seq xs x

--exercise 9
primes :: [Int]
primes = sieve [2..]

isPrime2 :: Int -> Bool
isPrime2 n = elem n (takeWhile (<= n) primes)


--exercise 10
--a
mean_helper :: [Double] -> Double -> Int -> Double
mean_helper [] s l = (s /fromIntegral l)
mean_helper (x:xs) s l = mean_helper xs (s+x) (l+1)


mean :: [Double] -> Double
mean xs = mean_helper xs 0 0

--b
mean_helper2 :: [Double] -> Double -> Int -> Double
mean_helper2 [] !s !l = (s /fromIntegral l)
mean_helper2 (x:xs) !s !l = mean_helper2 xs (s+x) (l+1)


mean2 :: [Double] -> Double
mean2 xs = mean_helper2 xs 0 0

--c
mean_and_var_helper :: [Double] -> Double -> Double -> Int -> (Double, Double)
mean_and_var_helper [] !s !ss !l = ((s /fromIntegral l), (ss / fromIntegral l) - ((s /fromIntegral l) * (s /fromIntegral l)))
mean_and_var_helper (x:xs) !s !ss !l = mean_and_var_helper xs (s+x) (ss+(x*x)) (l+1)


mean_and_var :: [Double] -> (Double, Double)
mean_and_var xs = mean_and_var_helper xs 0 0 0


main :: IO ()
main = do
  putStrLn "-- Ex 1 test --"
  print (goldbachPairs 10)

  putStrLn "\n-- Ex 2 test --"
  print (coprimePairs [1,2,3,4,5])

  putStrLn "\n-- Ex 3 test --"
  print (primesTo 20)
  print (isPrime 19)

  putStrLn "\n-- Ex 4 test --"
  print (matMul [[1,2],[3,4]] [[5,6],[7,8]])

  putStrLn "\n-- Ex 5 test --"
  print (permutations 2 [1,2,3])

  putStrLn "\n-- Ex 6 test --"
  print (take 20 hamming)

  putStrLn "\n-- Ex 7 test --"
  print (power 2 10)

  putStrLn "\n-- Ex 8 test --"
  print (listMax_bang [1,5,3,2,4])
  print (listMax_seq [1,5,3,2,4])

  putStrLn "\n-- Ex 9 test --"
  print (isPrime2 29)

  putStrLn "\n-- Ex 10 test --"
  print (mean [10,2,3,4,2,1,3,45,3,2,3,8])
  print (mean2 [10,2,3,4,2,1,3,45,3,2,3,8])
  print (mean_and_var [10,2,3,4,2,1,3,45,3,2,3,8])




