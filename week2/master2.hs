import Data.List
import System.Random

data CodePeg = Red | Green | Blue | Yellow | Orange | Brown deriving (Show, Eq)
data ScorePeg = Black | White | None deriving Show
data Score = Score ScorePeg ScorePeg ScorePeg ScorePeg deriving Show
type TurnCount = Int
type Code = String

nil :: Score
nil = Score None None None None

gameLength :: TurnCount
gameLength = 12

main :: IO ()
main = do
  generator <- getStdGen
  let code = take 4 $ randomRs ('0', '9') generator
  playGame code gameLength nil

playGame :: Code -> TurnCount -> Score -> IO ()
playGame code 0 _ = gameOver code
playGame code turnsLeft score= do
  guess <- ui code turnsLeft score
  let (correct, score') = check guess code
  if correct
  then putStrLn "You've cracked the code"
  else playGame code (turnsLeft-1) score'

gameOver :: Code -> IO ()
gameOver code = do
  putStrLn "You lose, loser"
  putStrLn $ "The code was : " ++ code

ui :: Code -> TurnCount -> Score -> IO Code
ui code turnsLeft score = do
  putStrLn $ show score 
  putStrLn $ show turnsLeft ++ " | Enter guess: "
  getLine

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


