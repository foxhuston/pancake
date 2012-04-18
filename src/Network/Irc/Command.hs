module Network.Irc.Command
        (Command
            (Ping,
             PrivMessage,
             GenericCommand,
             User,
             Join,
             Nick)) where

import Data.Maybe

type Nick = String

data (Eq a, Ord a) => Command a =
        Ping a |
        PrivMessage { from :: Nick, message :: a } |
        GenericCommand a |
        User a a a a |
        Join a |
        Nick a


    deriving (Show, Read, Ord, Eq)
