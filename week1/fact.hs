fact2 :: Int -> Int
fact2 0 = 1
fact2 n = n* fact2 (n -1)

mkNoble :: Bool -> String -> String
mkNoble True  name = "Dame " ++ name
mkNoble False name = "Sir " ++ name

