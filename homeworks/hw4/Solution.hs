module Hw4 (main) where

--exercise 1

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap f ra = Reader $ \env -> f (runReader ra env)

instance Applicative (Reader r) where
    pure x = Reader $ \_ -> x
    rf <*> ra = Reader $ \env -> runReader rf env (runReader ra env)

instance Monad (Reader r) where
    ra >>= f = Reader $ \env -> runReader (f (runReader ra env)) env

--exercise 2

ask :: Reader r r
ask = Reader $ \env -> env

asks :: (r -> a) -> Reader r a
asks f = Reader f

local :: (r -> r) -> Reader r a -> Reader r a
local f ra = Reader $ \env -> runReader ra (f env)

--exercise 3

data BankConfig = BankConfig
    { interestRate   :: Double
    , transactionFee :: Int
    , minimumBalance :: Int
    } deriving (Show)

data Account = Account
    { accountId :: String
    , balance   :: Int
    } deriving (Show)

calculateInterest :: Account -> Reader BankConfig Int
calculateInterest acc = do
    rate <- asks interestRate
    return $ floor (fromIntegral (balance acc) * rate)

applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee acc = do
    fee <- asks transactionFee
    return $ acc { balance = balance acc - fee }

checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance acc = do
    minBal <- asks minimumBalance
    return $ balance acc >= minBal

processAccount :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount acc = do
    updatedAcc <- applyTransactionFee acc
    interest   <- calculateInterest acc
    meetsMin   <- checkMinimumBalance acc
    return (updatedAcc, interest, meetsMin)

main :: IO ()
main = do
    let cfg = BankConfig { interestRate = 0.05, transactionFee = 2, minimumBalance = 100 }
    let acc = Account { accountId = "A-001", balance = 1000 }

    putStrLn "-- processAccount test --"
    print $ runReader (processAccount acc) cfg

    putStrLn "\n-- ask / asks test --"
    print $ runReader ask cfg
    print $ runReader (asks interestRate) cfg

    putStrLn "\n-- local test --"
    let doubleRate = local (\c -> c { interestRate = interestRate c * 2 })
    print $ runReader (doubleRate (calculateInterest acc)) cfg

    putStrLn "\n-- low balance test --"
    let poorAcc = acc { balance = 50 }
    print $ runReader (processAccount poorAcc) cfg