module Network.Irc.Parse (parse) where

import Network.Irc.Command
import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as P

parse = id

{-
parse :: String -> Command String
parse input = P.parse message "irc" input

-- :fox!~fox@localhost PRIVMSG #botTest :hey

message :: Parser (Command String)
message = try (do
            char ':'
            nick <- many1 (noneOf "!")
            manyTill anyChar (try (string "PRIVMSG #"))
            channel <- many1 letter
            string " :"
            message <- many (noneOf "\n")
            return $ PrivMessage nick message
        )
        <|>
        (do
            msg <- many1 anyChar
            return $ GenericCommand msg
        )
-}
