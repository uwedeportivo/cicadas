module EMPTrigger (blasts) where

import EMPTriggerInternal

blasts :: [Int] -> (Int -> Int) -> (Int, [Int])
blasts xs f = blastsImpl xs f
