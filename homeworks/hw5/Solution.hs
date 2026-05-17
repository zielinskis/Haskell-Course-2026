module Hw5 (main) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO

--exercise 1

data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG

execInstr :: Instr -> State [Int] ()
execInstr (PUSH n) = modify (n :)
execInstr POP = do
    s <- get
    case s of
        []     -> return ()
        (_:xs) -> put xs
execInstr DUP = do
    s <- get
    case s of
        []     -> return ()
        (x:xs) -> put (x : x : xs)
execInstr SWAP = do
    s <- get
    case s of
        (x:y:xs) -> put (y : x : xs)
        _        -> return ()
execInstr ADD = do
    s <- get
    case s of
        (x:y:xs) -> put (x + y : xs)
        _        -> return ()
execInstr MUL = do
    s <- get
    case s of
        (x:y:xs) -> put (x * y : xs)
        _        -> return ()
execInstr NEG = do
    s <- get
    case s of
        []     -> return ()
        (x:xs) -> put (negate x : xs)

execProg :: [Instr] -> State [Int] ()
execProg = mapM_ execInstr

runProg :: [Instr] -> [Int]
runProg instrs = execState (execProg instrs) []

--exercise 2

data Expr
    = Num Int
    | Var String
    | Add Expr Expr
    | Mul Expr Expr
    | Neg Expr
    | Assign String Expr
    | Seq    Expr Expr

eval :: Expr -> State (Map String Int) Int
eval (Num n)      = return n
eval (Var x)      = gets (Map.! x)
eval (Add e1 e2)  = (+) <$> eval e1 <*> eval e2
eval (Mul e1 e2)  = (*) <$> eval e1 <*> eval e2
eval (Neg e)      = negate <$> eval e
eval (Assign x e) = do
    v <- eval e
    modify (Map.insert x v)
    return v
eval (Seq e1 e2)  = eval e1 >> eval e2

runEval :: Expr -> Int
runEval e = evalState (eval e) Map.empty

--exercise 3

editDistM :: String -> String -> Int -> Int -> State (Map (Int, Int) Int) Int
editDistM xs ys i j = do
    cache <- get
    case Map.lookup (i, j) cache of
        Just v  -> return v
        Nothing -> do
            v <- compute
            modify (Map.insert (i, j) v)
            return v
  where
    compute
        | i == 0 = return j
        | j == 0 = return i
        | xs !! (i - 1) == ys !! (j - 1) = editDistM xs ys (i - 1) (j - 1)
        | otherwise = do
            del <- editDistM xs ys (i - 1) j
            ins <- editDistM xs ys i       (j - 1)
            sub <- editDistM xs ys (i - 1) (j - 1)
            return (1 + minimum [del, ins, sub])

editDistance :: String -> String -> Int
editDistance xs ys = evalState (editDistM xs ys (length xs) (length ys)) Map.empty

--exercises 4-6

data LocationType
    = Start
    | Decision [String]   -- list of path names
    | Obstacle Int        -- energy penalty
    | Treasure Int        -- points gained
    | Trap Int            -- points lost
    | Goal
    deriving (Show)

data Location = Location
    { locName     :: String
    , locType     :: LocationType
    , locNexts    :: [Int]     -- indices into the board
    } deriving (Show)

data GameState = GameState
    { playerPos    :: Int
    , playerScore  :: Int
    , playerEnergy :: Int
    , gameBoard    :: [Location]
    , gameOver     :: Bool
    } deriving (Show)

type AdventureGame a = StateT GameState IO a

-- The board
board :: [Location]
board =
    [ Location "Start"          Start                [1]         -- 0
    , Location "Forest Path"    (Decision ["left", "right"]) [2, 4]  -- 1
    , Location "Dark Cave"      (Obstacle 2)         [3]         -- 2
    , Location "Hidden Cache"   (Treasure 5)         [6]         -- 3
    , Location "Sunny Meadow"   (Treasure 3)         [5]         -- 4
    , Location "Quicksand"      (Trap 4)             [6]         -- 5
    , Location "Crossroads"     (Decision ["north", "south"]) [7, 8] -- 6
    , Location "Rocky Pass"     (Obstacle 3)         [9]         -- 7
    , Location "Ancient Ruins"  (Treasure 8)         [9]         -- 8
    , Location "Final Stretch"  (Trap 2)             [10]        -- 9
    , Location "Treasure"       Goal                 []          -- 10
    ]

initialState :: GameState
initialState = GameState
    { playerPos    = 0
    , playerScore  = 0
    , playerEnergy = 20
    , gameBoard    = board
    , gameOver     = False
    }

--exercise 6: IO helpers

getDiceRoll :: IO Int
getDiceRoll = do
    putStr "Roll the dice (1-6): "
    hFlush stdout
    input <- getLine
    let n = reads input :: [(Int, String)]
    case n of
        [(v, "")] | v >= 1 && v <= 6 -> return v
        _ -> do
            putStrLn "Invalid input, please enter a number between 1 and 6."
            getDiceRoll

displayGameState :: GameState -> IO ()
displayGameState gs = do
    let loc = gameBoard gs !! playerPos gs
    putStrLn $ "Location : " ++ locName loc
    putStrLn $ "Score    : " ++ show (playerScore gs)
    putStrLn $ "Energy   : " ++ show (playerEnergy gs)

getPlayerChoice :: [String] -> IO String
getPlayerChoice options = do
    putStrLn "Choose a path:"
    mapM_ (\(i, o) -> putStrLn $ "  " ++ show i ++ ") " ++ o) (zip [1..] options)
    putStr "Your choice: "
    hFlush stdout
    input <- getLine
    let n = reads input :: [(Int, String)]
    case n of
        [(v, "")] | v >= 1 && v <= length options -> return (options !! (v - 1))
        _ -> do
            putStrLn "Invalid choice, try again."
            getPlayerChoice options

--exercise 4

movePlayer :: Int -> AdventureGame Int
movePlayer roll = do
    gs <- get
    let pos   = playerPos gs
    let nexts = locNexts (gameBoard gs !! pos)
    let newPos = if null nexts then pos else head nexts
    modify (\s -> s { playerPos = newPos, playerEnergy = playerEnergy s - 1 })
    return roll

makeDecision :: [String] -> AdventureGame String
makeDecision options = do
    choice <- lift $ getPlayerChoice options
    gs <- get
    let pos   = playerPos gs
    let nexts = locNexts (gameBoard gs !! pos)
    let idx   = case lookup choice (zip options [0..]) of
                    Just i  -> i
                    Nothing -> 0
    let newPos = nexts !! idx
    modify (\s -> s { playerPos = newPos })
    return choice

--exercise 5

handleLocation :: AdventureGame Bool
handleLocation = do
    gs <- get
    let loc = gameBoard gs !! playerPos gs
    case locType loc of
        Goal -> do
            lift $ putStrLn "You reached the treasure!"
            modify (\s -> s { gameOver = True })
            return True
        Obstacle pen -> do
            lift $ putStrLn $ "Obstacle! You lose " ++ show pen ++ " energy."
            modify (\s -> s { playerEnergy = playerEnergy s - pen })
            return False
        Treasure pts -> do
            lift $ putStrLn $ "Treasure! You gain " ++ show pts ++ " points."
            modify (\s -> s { playerScore = playerScore s + pts })
            return False
        Trap pts -> do
            lift $ putStrLn $ "Trap! You lose " ++ show pts ++ " points."
            modify (\s -> s { playerScore = playerScore s - pts })
            return False
        Decision options -> do
            lift $ putStrLn $ "Decision point: " ++ locName loc
            _ <- makeDecision options
            return False
        Start -> return False

playTurn :: AdventureGame Bool
playTurn = do
    gs <- get
    if playerEnergy gs <= 0
        then do
            lift $ putStrLn "You ran out of energy. Game over!"
            modify (\s -> s { gameOver = True })
            return True
        else do
            let loc = gameBoard gs !! playerPos gs
            case locType loc of
                Decision _ -> do
                    lift $ displayGameState gs
                    handleLocation
                _ -> do
                    roll <- lift getDiceRoll
                    lift $ putStrLn $ "You rolled a " ++ show roll ++ "."
                    _ <- movePlayer roll
                    gs' <- get
                    lift $ displayGameState gs'
                    handleLocation

playGame :: AdventureGame ()
playGame = do
    gs <- get
    if gameOver gs
        then do
            lift $ putStrLn $ "Game ended. Final score: " ++ show (playerScore gs)
        else do
            ended <- playTurn
            if ended
                then do
                    gs' <- get
                    lift $ putStrLn $ "Game ended. Final score: " ++ show (playerScore gs')
                else playGame

main :: IO ()
main = do
    putStrLn "-- Stack machine --"
    print $ runProg [PUSH 3, PUSH 4, ADD]
    print $ runProg [PUSH 5, DUP, MUL]
    print $ runProg [PUSH 2, PUSH 3, SWAP, NEG, ADD]
    print $ runProg [POP]   -- empty stack, silently skipped

    putStrLn "\n-- Expression evaluator --"
    print $ runEval (Num 42)
    print $ runEval (Add (Num 3) (Mul (Num 4) (Num 5)))
    print $ runEval (Seq (Assign "x" (Num 10)) (Add (Var "x") (Num 5)))

    putStrLn "\n-- Edit distance --"
    print $ editDistance "kitten" "sitting"
    print $ editDistance "abc" "abc"
    print $ editDistance "" "hello"
    print $ editDistance "saturday" "sunday"

    putStrLn "\n-- Treasure Hunters --"
    runStateT playGame initialState
    return ()