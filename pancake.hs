module Main (main) where

import Control.Monad.Fix (fix)

import Data.List

import Network
import qualified Network.IRC as Irc

import System.Environment (getArgs)
import System.IO
import System.Random

import Text.Printf

import qualified Data.Markov as Markov

-- Funny: takeUntilStop $ generate' (mkStdGen 123) tr "this"

-- "fox is the minumum requirements for 8-bit computations.\""
-- "The battery life is alive!"

data (Ord a) => Sequence a = Start | Block a | End
        deriving (Eq, Ord, Read, Show)

type PancakeMap = Markov.MarkovMap (Sequence String)

chan   = "#optacular"
nick   = "pancake"

main :: IO ()
main = do
   args <- getArgs
   let server       = args !! 0
       port         = read (args !! 1)
   h <- connectTo server (PortNumber (fromIntegral port))
   training <- readFile "learned.map"
   rng <- getStdGen
   hSetBuffering h NoBuffering
   
   initCommands h "pancake"
   
   listen rng h $ read training

initCommands :: Handle -> String -> IO ()
initCommands h nick = do
   let nickCommand = Irc.nick nick
       userCommand = Irc.user nick "0" "*" nick
       joinCommand = Irc.joinChan "#botTest"
   mapM_ (write_ h . Irc.encode) [nickCommand, userCommand, joinCommand]

-- | Purely a utility function
mkTrainingMap :: String -> String -> IO ()
mkTrainingMap inPath outPath = do
   training <- readFile inPath
   let ls  = lines training
   let seq = concatMap mkSequenceLine ls
   let tr  = Markov.train seq
   writeFile outPath $ show tr

mkSequenceLine :: String -> [Sequence String]
mkSequenceLine line = Start : (map Block $ words line) ++ [End]

write_ :: Handle -> String -> IO ()
write_ h s = do
        hPrintf h "%s \r\n" s
        printf    "> %s \n" s

write :: Handle -> String -> String -> IO ()
write h s t = do
        hPrintf h "%s %s\r\n" s t
        printf    "> %s %s\n" s t

listen :: StdGen -> Handle -> PancakeMap -> IO ()
listen g h m = forever (g, m) $ \(rng, x) -> do
        t <- hGetLine h
        let s = init t
        (newRng, newM) <- (if ping s then (pong s >> return (g, m)) else eval rng h m (clean s))
        writeFile "learned.map" $ show newM
        putStrLn s
        return (newRng, newM)
    where
        forever x a = a x >>= (flip forever) a
        clean = drop 1 . dropWhile (/= ':') . drop 1
        ping x = "PING :" `isPrefixOf` x
        pong x = write h "PONG" (':' : drop 6 x)

eval :: StdGen -> Handle -> PancakeMap -> String -> IO (StdGen, PancakeMap)
eval g h m x | "pancake" `isInfixOf` x = do
        (gen, response) <- generate g m
        privmsg h response
        return (gen, m)
eval g _ m x = return (g, Markov.trainWithMap m $ mkSequenceLine x)

privmsg :: Handle -> String -> IO ()
privmsg h s = write h "PRIVMSG" (chan ++ " :" ++ s)

generate :: StdGen -> PancakeMap -> IO (StdGen, String)
generate rng markovMap = do
   let gen = takeWhile (isBlock.fst) $ drop 1 $ Markov.generate rng markovMap Start
   print gen
   let nextRand = snd $ last gen
   let seq = map fst gen
   return (nextRand, unSeq seq)

--- Utility functions ---

unSeq :: [Sequence String] -> String
unseq [] = ""
unseq ((Block x):xs) = x ++ " " ++ (unseq xs)
unSeq (_:xs) = unseq xs
unSeq _ = "wat"

isBlock :: Sequence String -> Bool
isBlock (Block _) = True
isBlock _         = False
