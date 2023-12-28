module Main (main) where

import qualified EMPTrigger (someFunc)

main :: IO ()
main = do
  putStrLn $ "Hello, Haskell: EMPTrigger.someFunc=" ++ show (EMPTrigger.someFunc 3)
