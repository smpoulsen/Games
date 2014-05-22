import Control.Applicative ((<$>), (<*>))
import qualified Data.Text as T
import System.IO (hFlush, stdout)
import System.Random 

type CorrectLetters = Int
type Difficulty   = Int
type GameState    = (Guesses, GoalWord, WordList)
type GoalWord     = T.Text
type GuessWord    = T.Text
type Guesses    = Int
type WordLength   = Int
type WordList     = [T.Text]
type WordNumber   = Int

wordLengthQuant :: StdGen -> Difficulty -> (WordLength, WordNumber)
wordLengthQuant g d = combos !! fst (randomR (0, length combos - 1) g)
  where combos = case d of
            1 -> possibleCombos [4..6]
            2 -> possibleCombos [6..8]
            3 -> possibleCombos [8..10]
            4 -> possibleCombos [10..12]
            5 -> possibleCombos [12..15]
            otherwise -> possibleCombos [4..15]
        possibleCombos z = (\x y -> (x,y)) <$> z <*> z

readWordList :: FilePath -> IO WordList
readWordList x = do
  corpus <- readFile x
  return . map T.pack . words $ corpus
    --where validWords = filter (\y -> T.length y <= 15 && T.length y >= 4)

getRandomWords :: WordList -> (WordLength, WordNumber) -> StdGen -> WordList
getRandomWords w (l,n) g = map (\x -> useableWords !! x) randomNs
  where randomNs = take n $ randomRs (0, length useableWords) g 
        wordsForDifficulty l' = filter (\x -> T.length x == l') 
        useableWords = wordsForDifficulty l w

checkGuess :: GoalWord -> GuessWord -> CorrectLetters
checkGuess goal guess = foldr (\(x,y) -> if x == y then (+1) else (+0)) 0 $ T.zip goal guess

main = do
  g         <- getStdGen
  gameState <- setUpGame g
  result    <- gameLoop gameState
  putStrLn "GAME OVER"
  
setUpGame :: StdGen -> IO GameState
setUpGame g = do
  difficulty  <- putStr "Difficulty (1-5)? " >> hFlush stdout >> readLn :: IO Int
  if difficulty `elem` [1..5]
  then do
    rawWordList    <- readWordList "enable1.txt"
    let wordLenNum = wordLengthQuant g difficulty
    let wordList   = map T.toUpper $ getRandomWords rawWordList wordLenNum g 
    goalIndex      <- randomRIO (0, length wordList - 1)
    let gameState  = (0, goalWord, wordList)
          where goalWord = wordList !! goalIndex
    mapM_ (putStrLn . T.unpack) wordList 
    return gameState
  else do
    putStrLn "Invalid difficulty. Choose again."
    setUpGame g


gameLoop :: GameState -> IO GameState
gameLoop (g, w, l) = do
  let remainingGuesses = 4 - g
  guess <- putStr ("Guess (" ++ show remainingGuesses ++ " left)? ") >> hFlush stdout >> getLine
  let formattedGuess = T.toUpper . T.pack $ guess
  if formattedGuess `elem` l 
  then
    if formattedGuess == w 
    then do
      putStrLn $ "\nYou won! The word was: " ++ T.unpack w 
      return (g, w, l) 
    else
      if g == 3
      then do 
        putStrLn $ "\nYou lost! The word was: " ++ T.unpack w
        return (g, w, l)
      else do
        let lettersCorrect = checkGuess w formattedGuess
        putStr $ show lettersCorrect ++ "/" ++ (show . T.length $ w) ++ " correct.\n"
        gameLoop (g+1, w, l)
  else do
    putStrLn "Invalid guess. Try again."
    gameLoop (g, w, l)