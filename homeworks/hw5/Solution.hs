module Solution where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)

-- 1) Stack machine
data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG

execInstr :: Instr -> State [Int] ()
execInstr (PUSH n) = modify (n :)
execInstr POP = do
    s <- get
    case s of
        _:rest -> put rest
        _      -> return ()
execInstr DUP = do
    s <- get
    case s of
        x:_ -> put (x : s)
        _   -> return ()
execInstr SWAP = do
    s <- get
    case s of
        x:y:rest -> put (y : x : rest)
        _        -> return ()
execInstr ADD = do
    s <- get
    case s of
        x:y:rest -> put ((y + x) : rest)
        _        -> return ()
execInstr MUL = do
    s <- get
    case s of
        x:y:rest -> put ((y * x) : rest)
        _        -> return ()
execInstr NEG = do
    s <- get
    case s of
        x:rest -> put ((-x) : rest)
        _      -> return ()

execProg :: [Instr] -> State [Int] ()
execProg = mapM_ execInstr

runProg :: [Instr] -> [Int]
runProg prog = execState (execProg prog) []

-- 2) Expression evaluator with variable bindings
data Expr
    = Num Int
    | Var String
    | Add Expr Expr
    | Mul Expr Expr
    | Neg Expr
    | Assign String Expr
    | Seq  Expr Expr

eval :: Expr -> State (Map String Int) Int
eval (Num n)         = return n
eval (Var name)      = gets (Map.findWithDefault 0 name)
eval (Add a b)       = (+) <$> eval a <*> eval b
eval (Mul a b)       = (*) <$> eval a <*> eval b
eval (Neg e)         = negate <$> eval e
eval (Assign name e) = do
    v <- eval e
    modify (Map.insert name v)
    return v
eval (Seq a b) = eval a >> eval b

runEval :: Expr -> Int
runEval e = evalState (eval e) Map.empty

-- 3) Memoised edit (Levenshtein) distance
editDistM :: String -> String -> Int -> Int -> State (Map (Int, Int) Int) Int
editDistM xs ys i j = do
    cache <- get
    case Map.lookup (i, j) cache of
        Just d  -> return d
        Nothing -> do
            d <- compute
            modify (Map.insert (i, j) d)
            return d
  where
    compute
        | i == 0 = return j
        | j == 0 = return i
        | xs !! (i - 1) == ys !! (j - 1) =
            editDistM xs ys (i - 1) (j - 1)
        | otherwise = do
            del <- editDistM xs ys (i - 1) j
            ins <- editDistM xs ys i (j - 1)
            sub <- editDistM xs ys (i - 1) (j - 1)
            return (1 + minimum [del, ins, sub])

editDistance :: String -> String -> Int
editDistance xs ys =
    evalState (editDistM xs ys (length xs) (length ys)) Map.empty

-- Treasure Hunters
data Tile
    = TStart
    | TEmpty
    | TDecision [(String, Int)]
    | TObstacle String Int
    | TTreasure String Int
    | TTrap String Int
    | TGoal
    deriving (Show)

data GameState = GameState
    { gsBoard    :: Map Int Tile
    , gsPos      :: Int
    , gsEnergy   :: Int
    , gsScore    :: Int
    , gsTurn     :: Int
    , gsBoardLen :: Int
    } deriving (Show)

type AdventureGame a = StateT GameState IO a

-- 4) Player movement and decisions
movePlayer :: Int -> AdventureGame Int
movePlayer roll = do
    gs <- get
    let newPos = min (gsPos gs + roll) (gsBoardLen gs)
        moved  = newPos - gsPos gs
    put gs { gsPos = newPos
           , gsEnergy = gsEnergy gs - 1
           , gsTurn = gsTurn gs + 1
           }
    return moved

makeDecision :: [String] -> AdventureGame String
makeDecision options = liftIO (getPlayerChoice options)

-- 5) Game loop
handleLocation :: AdventureGame Bool
handleLocation = do
    gs <- get
    let tile = Map.findWithDefault TEmpty (gsPos gs) (gsBoard gs)
    case tile of
        TGoal -> do
            liftIO (putStrLn "*** You reached the treasure! ***")
            return True
        TDecision opts -> do
            choice <- makeDecision (map fst opts)
            case lookup choice opts of
                Just target -> do
                    modify (\s -> s { gsPos = target })
                    liftIO (putStrLn ("You chose: " ++ choice))
                    handleLocation
                Nothing -> return False
        TObstacle desc penalty -> do
            liftIO (putStrLn ("Obstacle: " ++ desc ++ " (-" ++ show penalty ++ " energy)"))
            modify (\s -> s { gsEnergy = gsEnergy s - penalty })
            return False
        TTreasure desc bonus -> do
            liftIO (putStrLn ("Treasure: " ++ desc ++ " (+" ++ show bonus ++ " points)"))
            modify (\s -> s { gsScore = gsScore s + bonus })
            return False
        TTrap desc penalty -> do
            liftIO (putStrLn ("Trap: " ++ desc ++ " (-" ++ show penalty ++ " points)"))
            modify (\s -> s { gsScore = max 0 (gsScore s - penalty) })
            return False
        _ -> return False

playTurn :: AdventureGame Bool
playTurn = do
    gs <- get
    liftIO (displayGameState gs)
    if gsEnergy gs <= 0
        then do
            liftIO (putStrLn "*** You ran out of energy. Game over. ***")
            return True
        else do
            roll <- liftIO getDiceRoll
            moved <- movePlayer roll
            liftIO (putStrLn ("You moved " ++ show moved ++ " spaces."))
            handleLocation

playGame :: AdventureGame ()
playGame = do
    ended <- playTurn
    if ended then return () else playGame

-- 6) IO helpers
getDiceRoll :: IO Int
getDiceRoll = do
    putStr "Enter dice roll (1-6): "
    line <- getLine
    case reads line of
        [(n, "")] | n >= 1 && n <= 6 -> return n
        _ -> do
            putStrLn "Invalid roll. Try again."
            getDiceRoll

displayGameState :: GameState -> IO ()
displayGameState gs = do
    putStrLn "------------------------------"
    putStrLn ("Turn:     " ++ show (gsTurn gs))
    putStrLn ("Position: " ++ show (gsPos gs) ++ " / " ++ show (gsBoardLen gs))
    putStrLn ("Energy:   " ++ show (gsEnergy gs))
    putStrLn ("Score:    " ++ show (gsScore gs))
    putStrLn "------------------------------"

getPlayerChoice :: [String] -> IO String
getPlayerChoice options = do
    putStrLn "Choose one:"
    mapM_ (\(i, o) -> putStrLn ("  " ++ show i ++ ". " ++ o))
          (zip [1 :: Int ..] options)
    putStr "> "
    line <- getLine
    case reads line of
        [(n, "")] | n >= 1 && n <= length options ->
            return (options !! (n - 1))
        _ -> case filter (== line) options of
            [o] -> return o
            _   -> do
                putStrLn "Invalid choice. Try again."
                getPlayerChoice options

-- Sample board and entry point
initialGame :: GameState
initialGame = GameState
    { gsBoard = Map.fromList
        [ (0,  TStart)
        , (3,  TTreasure "A glittering coin" 10)
        , (5,  TObstacle "A fallen log" 2)
        , (7,  TTrap "Pit of doom" 5)
        , (8,  TDecision [("left", 12), ("right", 9)])
        , (9,  TObstacle "Rocky path" 1)
        , (11, TTreasure "Ancient amulet" 15)
        , (12, TTrap "Bandit ambush" 8)
        , (14, TTreasure "Final chest" 25)
        , (15, TGoal)
        ]
    , gsPos      = 0
    , gsEnergy   = 20
    , gsScore    = 0
    , gsTurn     = 0
    , gsBoardLen = 15
    }

playTreasureHunters :: IO ()
playTreasureHunters = do
    putStrLn "=== Treasure Hunters ==="
    (_, final) <- runStateT playGame initialGame
    putStrLn ("Final score: " ++ show (gsScore final))
