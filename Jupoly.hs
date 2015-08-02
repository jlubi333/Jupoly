module Main where

import Data.List (genericReplicate, intercalate)
import Control.Monad (when)
import System.Environment

foldLcm :: Integral i => [i] -> i
foldLcm = foldr1 lcm

flattenReplicate :: Integral i => i -> [a] -> [a]
flattenReplicate n = concat . genericReplicate n

toString :: Char -> String
toString c = c : ""

note :: Integral i => i -> i -> a -> a -> [a]
note beats n on off = on : genericReplicate (beats `div` n - 1) off

rhythm :: Integral i => i -> i -> a -> a -> [a]
rhythm beats n on off = flattenReplicate n $ note beats n on off

space :: Integral i => i -> [String] -> String
space s = intercalate (genericReplicate s ' ')

bar :: Integral i =>
    [i] -> [String] -> [String] -> String -> String -> i -> String
bar ns ons offs beatOn beatOff spacing =
    unlines (edge : mid ++ [edge])
  where
    beats = foldLcm ns
    edge  = space spacing $ (rhythm beats beats beatOn beatOff)
    mid   = zipWith3 (\n on off -> space spacing $ rhythm beats n on off)
                     ns ons offs

main :: IO ()
main = do
    args <- getArgs
    let len = length args
    if len < 1
      then putStrLn "Please pass in at least one argument."
      else do
        if len > 26
          then putStrLn "Please pass in less than twenty-six arguments."
          else do
            putStrLn $ bar (map read args)
                           (map toString ['a'..'z'])
                           (repeat " ")
                           "."
                           " "
                           2
