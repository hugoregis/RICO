module Main where

import Happstack.Server ( ServerPart, look, nullConf
                        , simpleHTTP, ok)


aad :: Int -> Int -> Int
aad x y = x + y

helloPart =
    do num1 <- look "num1"
       num2     <- look "num2"
       ok $ (show (aad (read num1) (read num2)))



main :: IO ()
main = simpleHTTP nullConf helloPart