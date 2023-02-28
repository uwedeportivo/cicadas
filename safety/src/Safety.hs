module Safety (
      binarySearch
    , findStep
) where

binarySearch :: (Int->Bool) -> Int -> Int -> Int
-- Preconditions:
-- predicate p has to be defined on domain [low, high)
-- low < high
-- predicate p has to satisfy: p(i) => (\forall low <= j < i: not p(j))
--                                        and
--                                      (\forall i <= j < high: p(j))
-- Return:
-- returns smallest index i with p(i),
-- if all values i in domain result in not p(i), then returns high
binarySearch p low _ | (p low) = low
binarySearch _ low high | (low + 1 == high) = high
binarySearch p low high = if (p m) then binarySearch p low m
                                   else binarySearch p m high 
        where m = low + (high - low) `div` 2


findStep' :: Double -> (Double -> Bool) -> Double -> Double -> Double
findStep' d _ low high | (high - low) < d = (high - low) / 2
findStep' d p low high = if (p m) then findStep' d p low m
                                  else findStep' d p m high
         where m = low + (high - low) / 2

findStep :: Double -> (Double -> Bool) -> Double
-- Preconditions:
-- predicate p has to be defined on domain [0,1]
-- predicate p has to satisfy: p(i) => (\forall low <= j < i: not p(j))
--                                        and
--                                      (\forall i <= j < high: p(j))
-- returns x such that y \in (x - d, x + d) with y = infinum{z: p(z)}
findStep _ p | (p 0.0) = 0.0
findStep _ p | (not (p 1.0)) = 1.0
findStep d p = findStep' d p 0.0 1.0