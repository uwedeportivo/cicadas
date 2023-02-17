module FindCycle (
    mycycle
  , fLength
  , fStart
  , fCycle
) where

mycycle :: [a] -> [a]
mycycle xs = foldr (:) (mycycle xs) xs

fLength :: Eq t => t -> [t] -> [t]
fLength x (y:ys)
    | x == y              = []
    | otherwise           = y:fLength x ys
fLength _ [] = []

fStart :: Eq a => [a] -> [a] -> ([a], [a])
fStart (x:xs) (y:ys)
    | x == y              = ([], x:fLength x xs)
    | otherwise           = let (as,bs) = fStart xs ys in (x:as,bs)
fStart _ [] = ([], [])
fStart [] _ = ([], [])

fCycle :: Eq a => [a] -> [a] -> [a] -> ([a], [a])
fCycle xxs (x:xs) (_:y:ys)
    | x == y              = fStart xxs xs
    | otherwise           = fCycle xxs xs ys
fCycle xxs _      _        = (xxs,[]) -- not cyclic
