{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Majority (majority) where

majority :: Eq a => [a] -> [a] -> a
majority [] (y:_) = y
majority (x:xs) [] = majority xs [x]
majority (x:xs) (y:ys) = 
    if x == y 
    then majority xs (x:y:ys) 
    else majority xs ys
