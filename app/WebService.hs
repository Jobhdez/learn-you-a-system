module Main where

sq :: Int -> Int
sq x = x * x

main :: IO ()
main = do
    let number = 5
    let result = sq number
    putStrLn ("The square of " ++ show number ++ " is " ++ show result)


