import System.Random  
  
main = do  
    gen <- getStdGen  
    print (randomPegs gen 4)


data CodePeg = Red | Green | Blue | Yellow | Orange | Brown deriving (Show, Eq)

codePegFromInt :: Int -> CodePeg
codePegFromInt n = convert (n `mod` 6)
  where convert 0 = Red
        convert 1 = Green
        convert 2 = Blue
        convert 3 = Yellow
        convert 4 = Orange
        convert 5 = Brown

randomPegs g n = map codePegFromInt $ take n (randomRs (0,6) g)
