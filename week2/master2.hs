-- ---------------------------------------------------------------------------
-- Imports
-- ---------------------------------------------------------------------------

import Data.Char
import Data.List
import System.Random

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

data CodePeg = Red | Green | Blue | Yellow | Orange | Purple deriving (Show, Eq)
data ScorePeg = Black | White | None deriving Show
data Score = Score ScorePeg ScorePeg ScorePeg ScorePeg deriving Show

type TurnCount = Int
type Code = [CodePeg]

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

nil :: Score
nil = Score None None None None

gameLength :: TurnCount
gameLength = 12

codeLength :: Int
codeLength = 4

numberColours :: Int
numberColours = 6

-- ---------------------------------------------------------------------------
-- Functions
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  generator <- getStdGen
  let code = randomPegs generator codeLength
  blurb
  playGame code gameLength nil

randomPegs :: RandomGen g => g -> Int -> [CodePeg]
randomPegs g n = map codePegFromInt $ take n (randomRs (0,numberColours) g)

codePegFromInt :: Int -> CodePeg
codePegFromInt n = convert (n `mod` numberColours)
  where convert 0 = Red
        convert 1 = Green
        convert 2 = Blue
        convert 3 = Yellow
        convert 4 = Orange
        convert 5 = Purple

blurb :: IO ()
blurb = do
  putStrLn "Guess the random 4 colour code to win!"
  putStrLn "Six colours to choose from!"
  putStrLn "Red, Green, Blue, Yellow, Orange, Purple"

playGame :: Code -> TurnCount -> Score -> IO ()
playGame code 0 _ = gameOver code
playGame code turnsLeft score= do
  userInput <- ui turnsLeft score
  let guess = codeFromString userInput
  let (correct, score') = check guess code
  if correct
  then putStrLn "You've cracked the code"
  else playGame code (turnsLeft-1) score'

gameOver :: Code -> IO ()
gameOver code = do
  putStrLn "You lose, loser"
  putStrLn $ "The code was : " ++ show code

ui :: TurnCount -> Score -> IO String
ui turnsLeft score = do
  putStrLn ""
  putStrLn $ show score 
  putStrLn $ show turnsLeft ++ " | Enter guess: "
  guess <- getLine
  if (length $ words guess) /= 4
  then do
    putStrLn "You must enter four colours"
    ui turnsLeft score
  else return guess

codeFromString :: String -> Code
codeFromString guess = map convert normalised
  where normalised = map (toUpper . head) $ words guess
        convert 'R' = Red
        convert 'G' = Green
        convert 'B' = Blue
        convert 'Y' = Yellow
        convert 'O' = Orange
        convert 'P' = Purple

check :: Code -> Code -> (Bool, Score)
check guess code = (cracked, score)
  where blackPegs = exactMatches guess code
        whitePegs = valueMatches guess code blackPegs
        score     = fromPegList $ makeScore blackPegs whitePegs
        cracked   = blackPegs == length code

exactMatches :: Code -> Code -> Int
exactMatches guess code = length . filter hit $ zip guess code
  where hit (a,b) = a == b

valueMatches :: Code -> Code -> Int -> Int
valueMatches guess code orderMatches = length code - (diffs + orderMatches)
  where diffs = length $ guess \\ code

makeScore :: Int -> Int -> [ScorePeg]
makeScore 0 0 = repeat None
makeScore 0 w = White : makeScore 0 (w-1)
makeScore b w = Black : makeScore (b-1) w

fromPegList :: [ScorePeg] -> Score
fromPegList (p1:p2:p3:p4:ps) = Score p1 p2 p3 p4


