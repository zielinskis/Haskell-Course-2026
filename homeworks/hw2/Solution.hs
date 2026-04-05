module Hw2 (main) where

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)

data Token = TNum Int | TAdd | TSub | TMul | TDiv

--exercise 1
instance Functor Sequence where
    fmap _ Empty = Empty
    fmap f (Single x) = Single (f x)
    fmap f (Append l r) = Append (fmap f l) (fmap f r)

--exercise 2
instance Foldable Sequence where
    foldMap _ Empty = mempty
    foldMap f (Single x) = f x
    foldMap f (Append l r) = foldMap f l <> foldMap f r

seqToList :: Sequence a -> [a]
seqToList = foldr (:) []

seqLength :: Sequence a -> Int
seqLength = foldl (\acc _ -> acc + 1) 0

--exercise 3
instance Semigroup (Sequence a) where
    (<>) = Append

instance Monoid (Sequence a) where
    mempty = Empty

--exercise 4
tailElem :: Eq a => a -> Sequence a -> Bool
tailElem x seq = go [seq]
  where
    go [] = False
    go (Empty : rest) = go rest
    go (Single y : rest) = y == x || go rest
    go (Append l r : rest) = go (l : r : rest)

--exercise 5
tailToList :: Sequence a -> [a]
tailToList seq = go [seq] []
  where
    go [] acc = reverse acc
    go (Empty : rest) acc = go rest acc
    go (Single x : rest) acc = go rest (x : acc)
    go (Append l r : rest) acc = go (l : r : rest) acc

--exercise 6
tailRPN :: [Token] -> Maybe Int
tailRPN tokens = go tokens []
  where
    go [] [result] = Just result
    go [] _ = Nothing
    go (TNum n : ts) stack = go ts (n : stack)
    go (op : ts) (b : a : stack) =
        case op of
            TAdd -> go ts (a + b : stack)
            TSub -> go ts (a - b : stack)
            TMul -> go ts (a * b : stack)
            TDiv -> if b == 0 then Nothing else go ts (a `div` b : stack)
    go _ _ = Nothing

--exercise 7
--a
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

--b
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p = foldr (\x acc -> if p x then x : acc else []) []

--c
decimal :: [Int] -> Int
decimal = foldl (\acc d -> acc * 10 + d) 0

--exercise 8
--a
encode :: Eq a => [a] -> [(a, Int)]
encode = foldr step []
  where
    step x ((y, n) : rest)
        | x == y = (y, n + 1) : rest
    step x acc = (x, 1) : acc

--b
decode :: [(a, Int)] -> [a]
decode = foldr (\(x, n) acc -> replicate n x ++ acc) []


main :: IO ()
main = do
    let s = Append (Append (Single 1) (Single 2)) (Append (Single 3) Empty)

    putStrLn "-- Ex 1 test --"
    print (seqToList (fmap (*2) s))

    putStrLn "\n-- Ex 2 test --"
    print (seqToList s)
    print (seqLength s)

    putStrLn "\n-- Ex 3 test --"
    print (seqToList (Single 1 <> Single 2 <> Single 3))

    putStrLn "\n-- Ex 4 test --"
    print (tailElem 2 s)
    print (tailElem 9 s)

    putStrLn "\n-- Ex 5 test --"
    print (tailToList s)

    putStrLn "\n-- Ex 6 test --"
    print (tailRPN [TNum 3, TNum 4, TAdd])
    print (tailRPN [TNum 10, TNum 2, TDiv])
    print (tailRPN [TNum 5, TNum 0, TDiv])
    print (tailRPN [TNum 1, TAdd])

    putStrLn "\n-- Ex 7 test --"
    print (myReverse [1,2,3,4,5])
    print (myTakeWhile even [2,4,3,6])
    print (decimal [1,2,3])

    putStrLn "\n-- Ex 8 test --"
    print (encode "aaabccca")
    print (decode [('a',3),('b',1),('c',3),('a',1)])