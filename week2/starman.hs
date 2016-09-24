check :: String -> String -> Char -> (Bool, String)
check word display c = (c `elem` word, [if x==c
                                        then c
                                        else y | (x,y) <- zip word display])

turn :: String -> String -> Int -> IO ()
turn word display n = do if n == 0
                         then putStrLn "You lose, loser!"
                         else if word == display
                              then putStrLn "You win this time punk..."
                              else mkGuess word display n

mkGuess :: String -> String -> Int -> IO ()
mkGuess word display n = 
  do putStrLn (display ++ " " ++ take n (repeat '*'))
     putStr "  Enter your guess: "
     q <- getLine
     let (correct, display') = check word  display (q!!0)
     let n' = if correct then n else n-1
     turn word display' n'

starman :: String -> Int -> IO ()
starman word n = turn word ['-' | x <- word] n


