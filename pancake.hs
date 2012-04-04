module Main (main) where

import System.Environment (getArgs)
import System.Random

import Control.Monad.Fix (fix)

import Data.List (isInfixOf)

import Data.Markov (MarkovMap)
import qualified Data.Markov as Markov

-- Funny: takeUntilStop $ generate' (mkStdGen 123) tr "this"

-- "fox is the minumum requirements for 8-bit computations.\""
-- "The battery life is alive!"

main :: IO ()
main = do
   args <- getArgs
   training <- readFile "training.txt"
   let tr = Markov.train $ words training
   randomGibberish <- generate (args !! 0) tr
   print randomGibberish

generate :: String -> MarkovMap String -> IO String
generate seed markovMap = do
   rng <- getStdGen
   return (seed ++ " " ++ (unwords $ takeUntilStop $ Markov.generate rng markovMap seed))

--- Utility functions ---

hasAny :: Eq a => [a] -> [a] -> Bool
hasAny [] ys     = False
hasAny (x:xs) ys = x `elem` ys || hasAny xs ys

takeUntilStop :: [String] -> [String]
takeUntilStop = takeUntil (not . hasAny ".!?")

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ []     = []
takeUntil f (x:xs) =
   case f x of
	True -> x : (takeUntil f xs)
	False -> [x]
