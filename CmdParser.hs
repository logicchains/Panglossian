module Panglossian.CmdParser (parseCommand) where

import Control.Concurrent.STM
import Data.Either
import Data.List
import Data.Int

data Token = NumToken Int32 | StrToken String
           deriving Show

type Command = (String, [Token])

commands = ["listen"]

parseInt :: String -> Maybe Int32
parseInt str = case (reads str :: [(Int32, String)]) of
    [] -> Nothing
    [(num, _)] -> Just num

expectToken :: Token -> String -> Either String Token
expectToken (StrToken _) str = Right $ StrToken str
expectToken (NumToken _) str = case (parseInt str) of
                                  Just n -> Right $ NumToken n
                                  Nothing -> Left ("Error: "++str++" is not an int.")

expectTokens :: [Token] -> [String] -> ([String], [Token])
expectTokens expected actual = partitionEithers $ zipWith expectToken expected actual

sendCommand :: Command -> TChan Command -> IO()
sendCommand cmd chan = atomically $ writeTChan chan cmd

parseCommand :: [String] -> Either String Command
parseCommand [] = Left "Error: no command entered\n" 
parseCommand [x] = if elem x commands then Right (x, []) else Left ("Error: invalid command: " ++ x)
