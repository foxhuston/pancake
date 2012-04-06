module Main (main) where

import Network

import System.Environment (getArgs)
import System.IO
import System.Random

import Control.Monad.Fix (fix)

import Data.List

import Text.Printf

import qualified Data.Markov as Markov

-- Funny: takeUntilStop $ generate' (mkStdGen 123) tr "this"

-- "fox is the minumum requirements for 8-bit computations.\""
-- "The battery life is alive!"

data (Ord a) => Sequence a = Start | Block a | End
        deriving (Eq, Ord, Read, Show)

type PancakeMap = Markov.MarkovMap (Sequence String)

server = ""
port   = 7000
chan   = "#botTest"
nick   = "pancake"

main :: IO ()
main = do
   args <- getArgs
   h <- connectTo server (PortNumber (fromIntegral port))
   training <- readFile "training.map"
   hSetBuffering h NoBuffering
   write h "NICK" nick
   write h "USER" (nick++" 0 * :pancake")
   write h "JOIN" chan
   listen h $ read training

-- | Purely a utility function
mkTrainingMap :: String -> IO ()
mkTrainingMap path = do
   training <- readFile "training.txt"
   let ls  = lines training
   let seq = concatMap mkSequenceLine ls
   let tr  = Markov.train seq
   writeFile path $ show tr

mkSequenceLine :: String -> [Sequence String]
mkSequenceLine line = Start : (map Block $ words line) ++ [End]

write :: Handle -> String -> String -> IO ()
write h s t = do
        hPrintf h "%s %s\r\n" s t
        printf    "> %s %s\n" s t

listen :: Handle -> PancakeMap -> IO ()
listen h m = forever m $ \x -> do
        t <- hGetLine h
        let s = init t
        newM <- eval h m (clean s)
        writeFile "learned.map" $ show newM
        putStrLn s
        return newM
    where
        forever x a = a x >>= (flip forever) a

        clean = drop 1 . dropWhile (/= ':') . drop 1

        ping x = "PING :" `isPrefixOf` x
        pong x = write h "PONG" (':' : drop 6 x)

eval :: Handle -> PancakeMap -> String -> IO PancakeMap
eval h m x | "pancake" `isInfixOf` x = do
        response <- generate m
        privmsg h response
        return m
eval h m x | "PING :" `isPrefixOf` x = write h "PONG" (':' : drop 6 x) >> return m
eval _ m x = return $ Markov.trainWithMap m $ mkSequenceLine x

privmsg :: Handle -> String -> IO ()
privmsg h s = write h "PRIVMSG" (chan ++ " :" ++ s)

generate :: PancakeMap -> IO String
generate markovMap = do
   rng <- getStdGen
   return $ unSeq $ takeWhile isBlock $ drop 1 $ Markov.generate rng markovMap Start

--- Utility functions ---

unSeq :: [Sequence String] -> String
unseq [] = ""
unseq ((Block x):xs) = x ++ " " ++ (unseq xs)
unSeq (_:xs) = unseq xs

isBlock :: Sequence String -> Bool
isBlock (Block _) = True
isBlock _         = False
