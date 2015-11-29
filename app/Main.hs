module Main where

import Language.Lambda
import System.IO (interact)

main :: IO ()
main = interact process

process :: String -> String
process = 
  let evalR  = fmap (eval empty')
  in handleError . evalR . runLambdaParser

