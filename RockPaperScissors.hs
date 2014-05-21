-------------------- Imports --------------------
import Control.Applicative 
import Data.Char (toLower)
import System.Random
import System.IO

-------------------- Data & Types --------------------
data Move = Rock | Paper | Scissors | Lizard | Spock deriving (Eq, Bounded, Show, Read, Enum)

instance Ord Move where
    compare x y 
        | x == Rock     && y `elem` [Scissors, Lizard] = GT
        | x == Paper    && y `elem` [Rock, Spock]      = GT
        | x == Scissors && y `elem` [Paper, Lizard]    = GT
        | x == Lizard   && y `elem` [Spock, Paper]     = GT
        | x == Spock    && y `elem` [Scissors, Rock]   = GT
        | otherwise                                    = LT

instance Random Move where
    random g = case randomR (fromEnum (minBound :: Move), fromEnum (maxBound :: Move)) g of
        (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
        (r, g') -> (toEnum r, g')

data Score = Win | Loss | Tie deriving (Eq, Ord)

type Stats = (Int, Int, Int)

instance Show Score where
    show x = case x of
        Win  -> "Player wins!"
        Loss -> "Computer wins!"
        Tie  -> "Tie game!" 

-------------------- Functions --------------------

setMove :: String -> Maybe Move
setMove x = case map toLower x of
    "rock"     -> Just Rock
    "paper"    -> Just Paper
    "scissors" -> Just Scissors
    "lizard"   -> Just Lizard
    "spock"    -> Just Spock
    otherwise  -> Nothing

playGame :: Move -> Move -> Score
playGame x y 
    | x == y    = Tie
    | x < y     = Win
    | otherwise = Loss

updateStats :: Stats -> Score -> Stats
updateStats (w,l,t) y = case y of
    Win  -> (w+1, l,   t)
    Loss -> (w,   l+1, t)
    Tie  -> (w,   l,   t+1)

printStats :: Stats -> String
printStats (w,l,t) = let wins   = fromIntegral w
                         losses = fromIntegral l
                         ties   = fromIntegral t
                         games  = wins + losses + ties
                         winP   = wins / games
                         lossP  = losses / games
                         tieP   = ties / games
    in "Wins: " ++ show wins ++ " (" ++ show winP ++ "), Losses: " ++ show losses ++ 
            " (" ++ show lossP ++ "), Ties: " ++ show ties ++ " (" ++ show tieP ++ ")"

main = do
    g     <- getStdGen
    stats <- game g (0, 0, 0)
    putStrLn $ "\nGame over. Final tally:\n" ++ printStats stats

game :: StdGen -> Stats -> IO Stats
game g stats = do

    putStr "\nPlayer chooses: "
    hFlush stdout
    playerRaw <- getLine 
    
    let player   = setMove playerRaw
    let computer = random g :: (Move, StdGen)
    let result   = playGame (fst computer) <$> player
    
    case result of
        Nothing -> do
            putStrLn "Invalid move."
            game (snd computer) stats
        Just x  -> do
            putStrLn $ "Computer chose: " ++ (show . fst $ computer)
            putStrLn $ show x
      
            let newStats = updateStats stats x
            putStrLn $ "\n" ++ printStats newStats
            putStr $ "Continue? [y/n]: "
            hFlush stdout
            continue <- getLine
            case continue of
                "n" -> return newStats
                otherwise -> game (snd computer) newStats