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
blasted r f xs n = (mp, (n:iz))
    where
        x = xs !! (n - 1)
        g :: (Vector (Int, [Int])) -> Int -> Int
        g rr j = ((fst (rr!j))) + (min x (f (n - j)))
        ps = [(g r j) | j <- [0..(n-1)]]
        (mp, i) = maxInListWithIndex ps
        iz = snd (r!i)

blastsImpl :: [Int] -> (Int -> Int) -> (Vector (Int, [Int]))
blastsImpl xs f = r
    where
        r = generate ((length xs) + 1) (blasted r f xs)

overall :: (Vector (Int, [Int])) -> (Vector (Int, [Int])) -> Int -> (Int, [Int])
overall _ _  0 = (0, [])
overall o r n = if b > c then (b, biz) else (c, oiz)
    where
        (b, biz) = r ! n
        (c, oiz) = o ! (n - 1)

overallImpl :: [Int] -> (Int -> Int) -> (Int, [Int])
overallImpl xs f = o ! n
    where
        n = length xs
        r = blastsImpl xs f
        o = generate ((length xs) + 1) (overall o r)
