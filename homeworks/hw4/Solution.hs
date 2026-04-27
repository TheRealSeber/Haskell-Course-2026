module Solution where

newtype Reader r a = Reader { runReader :: r -> a }

-- 1) Functor, Applicative, Monad instances
instance Functor (Reader r) where
    fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader r) where
    pure x = Reader (\_ -> x)
    liftA2 f (Reader ra) (Reader rb) = Reader (\r -> f (ra r) (rb r))

instance Monad (Reader r) where
    Reader ra >>= f = Reader (\r -> runReader (f (ra r)) r)

-- 2) Primitive operations
ask :: Reader r r
ask = Reader id

asks :: (r -> a) -> Reader r a
asks = Reader

local :: (r -> r) -> Reader r a -> Reader r a
local f (Reader g) = Reader (g . f)

-- 3) Banking example
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
    return (floor (fromIntegral (balance acc) * rate))

applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee acc = do
    fee <- asks transactionFee
    return acc { balance = balance acc - fee }

checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance acc = do
    minBal <- asks minimumBalance
    return (balance acc >= minBal)

processAccount :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount acc = do
    feeAcc   <- applyTransactionFee acc
    interest <- calculateInterest acc
    okMin    <- checkMinimumBalance acc
    return (feeAcc, interest, okMin)
