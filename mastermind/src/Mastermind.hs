module Mastermind (
    Color
  , Score
  , colors
  , eval
  , filterGuesses
  , Round
  , play
  , solve
) where

import Data.List ( sort )

data Color = White | Pink | Yellow | Green | Red | Blue deriving (Eq, Show, Ord)

colors :: [Color]
colors = [White, Pink, Yellow, Green, Red, Blue]

type Score = (Int, Int)


mismatchMuncher :: (Color, Color) -> [(Color, Color)] -> [(Color, Color)]

mismatchMuncher (c1, c2) ms = if c1 /= c2 then (c1, c2):ms else ms

mismatches :: [Color] -> [Color] -> [(Color, Color)]

mismatches code guess = foldr mismatchMuncher []  (zip code guess)

countColorMatches :: [Color] -> [Color] -> Int

countColorMatches _ [] = 0
countColorMatches [] _ = 0
countColorMatches (x:xs) (y:ys) | x < y = countColorMatches xs (y:ys)
                                | x > y = countColorMatches (x:xs) ys
                                | otherwise = 1 + (countColorMatches xs ys)

eval :: [Color] -> [Color] -> Score

eval code guess = (correctPositions, correctColors)
  where
    ms = mismatches code guess
    correctPositions = length code - length ms
    (codeColors, guessColors) = unzip ms
    correctColors = countColorMatches (sort codeColors) (sort guessColors)


filterGuesses :: [[Color]] -> [Color] -> Score -> [[Color]]

filterGuesses gs guess score = filter (\guess' -> (eval guess guess') == score) gs

type Round = (
    [Color]    -- code
  , [[Color]]  -- guesses
  , Score      -- evaluation of (head guesses) against code
  )

play :: Round -> Round

play (code, guesses, _) = (code, remainingGuesses, score)
    where
        guess = head guesses
        score = eval code guess
        remainingGuesses = filterGuesses (tail guesses) guess score


leftPad :: a -> Int -> [a] -> [a]

leftPad _ 0 xs = xs
leftPad x n xs = x:leftPad x (n-1) xs

digits :: Int -> Int -> [Int]

digits x base = digitMuncher x []
  where
    digitMuncher :: Int -> [Int] -> [Int]
    digitMuncher 0 ds = ds
    digitMuncher y ds =  digitMuncher (div y base) ((mod y base):ds)

ensureLength :: a -> Int -> [a] -> [a]

ensureLength x n xs = if l < n then leftPad x (n - l) xs else xs
    where
        l = length xs

colorDigits :: Int -> Int -> [Color]

colorDigits m n = map (colors !!) ds
    where
        base = length colors
        ds = ensureLength 0 m (digits n base)

allPossibleGuesses :: Int -> [[Color]]

allPossibleGuesses n = [ colorDigits n y | y <- [0..e]]
    where
        e = (length colors)^n - 1

solve :: [Color] -> [[Color]]
solve code = map (\(_, gs, _) -> (head gs)) solution
  where
    guesses = allPossibleGuesses (length code)
    rounds = iterate play (code, guesses, (0, 0))
    solution = takeWhile (\(_, gs, _) -> gs /= []) rounds
