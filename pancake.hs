module Main (main) where

import Prelude hiding (foldr)
import Data.Foldable (foldr)
import Data.List (sort)
import Data.Map ((!))
import qualified Data.Map as Map
import System.Environment (getArgs)
import System.Random

type MarkovMap a = Map.Map a (Map.Map a Int) 

-- Funny: takeUntilStop $ generate' (mkStdGen 123) tr "this"

-- "fox is the minumum requirements for 8-bit computations.\""
-- "The battery life is alive!"

main :: IO ()
main = do
   args <- getArgs
   training <- readFile "training.txt"
   let tr = train $ words training
   randomGibberish <- generate (args !! 0) tr
   print randomGibberish

generate :: String -> MarkovMap String -> IO String
generate seed markovMap = do
   rng <- getStdGen
   return (seed ++ " " ++ (unwords $ takeUntilStop $ generate' rng markovMap seed))

generate' :: (Ord a) => StdGen -> MarkovMap a -> a -> [a]
generate' randomGen m current =
    let currentMap    = m ! current
    	total         = foldr (+) 0 currentMap
        normalized    = Map.map ((/(fromIntegral total)).fromIntegral) currentMap
	skippableList = Map.foldrWithKey (\k v vs -> (v, k):vs) [] normalized
	sortedList    = reverse $ sort skippableList
	(rand, next)  = getRandomDouble randomGen
	element	      = getNearest rand sortedList
	rest	      = generate' next m element
    in element : rest

getRandomDouble :: (RandomGen g) => g -> (Double, g)
getRandomDouble = random

getNearest :: Double -> [(Double, a)] -> a
getNearest counter ((number, item):xs) =
    case compare n 0 of
	GT -> getNearest n xs
	EQ -> item
	LT -> item
    where n = counter - number

train :: (Ord a) => [a] -> MarkovMap a
train xs = train' (zipWith (,) xs $ drop 1 xs) Map.empty

train' :: (Ord a) => [(a, a)] -> MarkovMap a -> MarkovMap a
train' [] m = m
train' ((first, follow):xs) m =
    let innerMap  = Map.findWithDefault (Map.singleton follow 0) first m
    	innerMap' = Map.insertWith (+) follow 1 innerMap
    in train' xs (Map.insert first innerMap' m)


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
