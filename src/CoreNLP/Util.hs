module CoreNLP.Util (firstToUpper) where

import Data.Char

firstToUpper :: String -> String
firstToUpper (x:xs) = (toUpper x):xs
firstToUpper [] = []
