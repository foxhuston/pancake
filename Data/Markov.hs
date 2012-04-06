module Data.Markov (MarkovMap
	           ,generate
		   ,train
		   ,trainWithMap)
    where

import Prelude hiding (foldr)
import Data.Foldable (foldr)
import Data.List (sort)
import Data.Map ((!))
import qualified Data.Map as Map
import System.Random

type MarkovMap a = Map.Map a (Map.Map a Int) 

generate :: (Ord a) => StdGen -> MarkovMap a -> a -> [(a, StdGen)]
generate randomGen m current =
    let currentMap    = m ! current
    	total         = foldr (+) 0 currentMap
        normalized    = Map.map ((/(fromIntegral total)).fromIntegral) currentMap
	skippableList = Map.foldrWithKey (\k v vs -> (v, k):vs) [] normalized
	sortedList    = reverse $ sort skippableList
	(rand, next)  = getRandomDouble randomGen
	element	      = getNearest rand sortedList
	rest	      = generate next m element
    in (element, next) : rest

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
train = trainWithMap Map.empty

trainWithMap :: (Ord a) => MarkovMap a -> [a] -> MarkovMap a
trainWithMap m xs = train' (zipWith (,) xs $ drop 1 xs) m

train' :: (Ord a) => [(a, a)] -> MarkovMap a -> MarkovMap a
train' [] m = m
train' ((first, follow):xs) m =
    let innerMap  = Map.findWithDefault (Map.singleton follow 0) first m
    	innerMap' = Map.insertWith (+) follow 1 innerMap
    in train' xs (Map.insert first innerMap' m)
