module Main (main) where

import Network

import System.Environment (getArgs)
import System.IO
import System.Random

import Control.Monad.Fix (fix)

import Data.List

import Text.Printf

import Data.Markov (MarkovMap)
import qualified Data.Markov as Markov

-- Funny: takeUntilStop $ generate' (mkStdGen 123) tr "this"

-- "fox is the minumum requirements for 8-bit computations.\""
-- "The battery life is alive!"

server = ""
port   = 7000
chan   = "#botTest"
nick   = "pancake"

main :: IO ()
main = do
   args <- getArgs
   training <- readFile "training.txt"
   let tr = Markov.train $ words training
   h <- connectTo server (PortNumber (fromIntegral port))
   hSetBuffering h NoBuffering
   write h "NICK" nick
   write h "USER" (nick++" 0 * :pancake")
   write h "JOIN" chan
   listen h tr


write :: Handle -> String -> String -> IO ()
write h s t = do
        hPrintf h "%s %s\r\n" s t
        printf    "> %s %s\n" s t

listen :: Handle -> MarkovMap String -> IO ()
listen h m = forever m $ \x -> do
        t <- hGetLine h
        let s = init t
        newM <- eval h m (clean s)
        putStrLn s
        return newM
    where
        forever x a = a x >>= (flip forever) a

        clean = drop 1 . dropWhile (/= ':') . drop 1

        ping x = "PING :" `isPrefixOf` x
        pong x = write h "PONG" (':' : drop 6 x)

eval :: Handle -> MarkovMap String -> String -> IO (MarkovMap String)
eval h m x | "pancake" `isInfixOf` x = do
        response <- generate "this" m
        privmsg h response
        return m
eval h m x | "PING :" `isPrefixOf` x = write h "PONG" (':' : drop 6 x) >> return m
eval _ m x = return $ Markov.trainWithMap m [x]

privmsg :: Handle -> String -> IO ()
privmsg h s = write h "PRIVMSG" (chan ++ " :" ++ s)

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
