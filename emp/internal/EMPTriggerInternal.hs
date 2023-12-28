module EMPTriggerInternal where
    
import Data.Vector (Vector, (!), generate)

maxInListWithIndex :: [Int] -> (Int, Int)
maxInListWithIndex [] = (0, -1)
maxInListWithIndex [x] = (x, 0)
maxInListWithIndex (x:xs) = if x > y then (x, 0) else (y, (i+1))
  where
    (y, i) = maxInListWithIndex xs

blasted :: (Vector (Int, [Int])) -> (Int -> Int) -> [Int] -> Int -> (Int, [Int])
blasted _ _ [] _ = (0, [])
blasted _ _ _ 0 = (0, [])
blasted r f xs n = w
    where
        x = xs !! (n - 1)
        g :: (Vector (Int, [Int])) -> Int -> Int
        g rr j = ((fst (rr!j))) + (min x (f (n - j)))
        ps = [(g r j) | j <- [0..(n-1)]]
        (mp, i) = maxInListWithIndex ps
        v = fst (r!(n-1))
        iz = snd (r!i)
        w = if mp > v then (mp, (n:iz)) else (r!(n-1))

blastsImpl :: [Int] -> (Int -> Int) -> (Int, [Int])
blastsImpl [] _ = (0, [])
blastsImpl xs f = r ! n
  where
    n = length xs
    r = generate ((length xs) + 1) (blasted r f xs)
