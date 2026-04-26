module Hw3 (main) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (permutations)
import Control.Monad (guard, mapM)
import Control.Monad.Writer (Writer, tell, runWriter)

-- types used across exercises
type Pos      = (Int, Int)
data Dir      = N | S | E | W deriving (Eq, Ord, Show)
type Maze     = Map Pos (Map Dir Pos)
type Key      = Map Char Char
type Guest    = String
type Conflict = (Guest, Guest)
data Expr     = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr deriving (Show)

--exercise 1
--a
move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = do
    neighbours <- Map.lookup pos maze
    Map.lookup dir neighbours

--b
followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath maze start dirs = foldl step (Just start) dirs
  where
    step acc dir = do
        pos <- acc
        move maze pos dir

--c
safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath maze start dirs = fmap (start :) (go start dirs)
  where
    go _ []       = Just []
    go pos (d:ds) = do
        next <- move maze pos d
        rest <- go next ds
        return (next : rest)

--exercise 2
decrypt :: Key -> String -> Maybe String
decrypt key = traverse (\c -> Map.lookup c key)

decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key = traverse (decrypt key)

--exercise 3
seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings guests conflicts = do
    perm <- permutations guests
    let pairs = zip perm (tail perm ++ [head perm])
    guard (all (not . isConflict) pairs)
    return perm
  where
    isConflict (a, b) = (a, b) `elem` conflicts || (b, a) `elem` conflicts

--exercise 4
data Result a = Failure String | Success a [String]

--a
instance Functor Result where
    fmap _ (Failure msg)       = Failure msg
    fmap f (Success val ws)    = Success (f val) ws

instance Applicative Result where
    pure x = Success x []
    Failure msg     <*> _               = Failure msg
    _               <*> Failure msg     = Failure msg
    Success f ws1   <*> Success x ws2   = Success (f x) (ws1 ++ ws2)

instance Monad Result where
    return = pure
    Failure msg     >>= _ = Failure msg
    Success x ws    >>= f =
        case f x of
            Failure msg      -> Failure msg
            Success y ws'    -> Success y (ws ++ ws')

--b
warn :: String -> Result ()
warn msg = Success () [msg]

failure :: String -> Result a
failure = Failure

--c
validateAge :: Int -> Result Int
validateAge age
    | age < 0   = failure "Age cannot be negative"
    | age > 150 = do warn "Age is suspiciously high"; return age
    | otherwise = return age

validateAges :: [Int] -> Result [Int]
validateAges = mapM validateAge

--exercise 5
simplify :: Expr -> Writer [String] Expr
simplify (Lit n) = return (Lit n)
simplify (Neg e) = do
    e' <- simplify e
    case e' of
        Neg inner -> do
            tell ["Double negation: neg (neg e) -> e"]
            return inner
        _ -> return (Neg e')
simplify (Add l r) = do
    l' <- simplify l
    r' <- simplify r
    case (l', r') of
        (Lit 0, _)      -> do tell ["Add identity: 0 + e -> e"]; return r'
        (_, Lit 0)      -> do tell ["Add identity: e + 0 -> e"]; return l'
        (Lit a, Lit b)  -> do tell ["Constant folding: " ++ show a ++ " + " ++ show b ++ " -> " ++ show (a+b)]; return (Lit (a+b))
        _               -> return (Add l' r')
simplify (Mul l r) = do
    l' <- simplify l
    r' <- simplify r
    case (l', r') of
        (Lit 0, _)      -> do tell ["Zero absorption: 0 * e -> 0"]; return (Lit 0)
        (_, Lit 0)      -> do tell ["Zero absorption: e * 0 -> 0"]; return (Lit 0)
        (Lit 1, _)      -> do tell ["Mul identity: 1 * e -> e"]; return r'
        (_, Lit 1)      -> do tell ["Mul identity: e * 1 -> e"]; return l'
        (Lit a, Lit b)  -> do tell ["Constant folding: " ++ show a ++ " * " ++ show b ++ " -> " ++ show (a*b)]; return (Lit (a*b))
        _               -> return (Mul l' r')


main :: IO ()
main = do
    -- Ex 1
    let maze :: Maze
        maze = Map.fromList
            [ ((0,0), Map.fromList [(E, (1,0)), (S, (0,1))])
            , ((1,0), Map.fromList [(W, (0,0)), (S, (1,1))])
            , ((0,1), Map.fromList [(N, (0,0)), (E, (1,1))])
            , ((1,1), Map.fromList [(N, (1,0)), (W, (0,1))])
            ]

    putStrLn "-- Ex 1 test --"
    print (move maze (0,0) E)
    print (move maze (0,0) N)
    print (followPath maze (0,0) [E, S])
    print (followPath maze (0,0) [N, E])
    print (safePath maze (0,0) [E, S])
    print (safePath maze (0,0) [N])

    -- Ex 2
    let key = Map.fromList [('a','x'), ('b','y'), ('c','z')]

    putStrLn "\n-- Ex 2 test --"
    print (decrypt key "abc")
    print (decrypt key "abcd")
    print (decryptWords key ["ab", "bc"])
    print (decryptWords key ["ab", "cd"])

    -- Ex 3
    putStrLn "\n-- Ex 3 test --"
    let guests    = ["Alice", "Bob", "Carol", "Dave"]
        conflicts = [("Alice", "Bob")]
    print (seatings guests conflicts)

    -- Ex 4
    putStrLn "\n-- Ex 4 test --"
    print (case validateAge 25  of { Failure m -> Left m; Success v ws -> Right (v, ws) })
    print (case validateAge (-1) of { Failure m -> Left m; Success v ws -> Right (v, ws) })
    print (case validateAge 200 of { Failure m -> Left m; Success v ws -> Right (v, ws) })
    print (case validateAges [20, 200, 30] of { Failure m -> Left m; Success v ws -> Right (v, ws) })

    -- Ex 5
    putStrLn "\n-- Ex 5 test --"
    let expr1 = Add (Lit 0) (Mul (Lit 1) (Lit 3))
        expr2 = Neg (Neg (Add (Lit 2) (Lit 3)))
        (r1, log1) = runWriter (simplify expr1)
        (r2, log2) = runWriter (simplify expr2)
    print r1
    mapM_ putStrLn log1
    print r2
    mapM_ putStrLn log2