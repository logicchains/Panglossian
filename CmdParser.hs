module Panglossian.CmdParser (awaitCommands, runPrinter, Command, Token(..)) where

import Control.Concurrent.STM
import Data.Functor
import Data.Either
import qualified Data.List as L
import Data.Int
import Data.Word
import System.IO

data Token = NumToken Int32 | StrToken String
           deriving Show

type Command = (String, [Token])

parseInt :: String -> Maybe Int32
parseInt str = case (reads str :: [(Int32, String)]) of
    [] -> Nothing
    [(num, _)] -> Just num

expectToken :: Token -> String -> Either String Token
expectToken (StrToken _) str = Right $ StrToken str
expectToken (NumToken _) str = case parseInt str of
                                  Just n -> Right $ NumToken n
                                  Nothing -> Left ("Error: "++str++" is not an integer.")

expectTokens :: [Token] -> [String] -> Either [String] [Token]
expectTokens expected actual
    | L.length expected /= L.length actual =
        Left ["Error: expected " ++ show (L.length expected) ++ " arguments"]
    | null errs = Right toks
    | otherwise = Left errs
    where (errs, toks) = partitionEithers $ zipWith expectToken expected actual

sendCommand :: Command -> TChan Command -> IO()
sendCommand cmd chan = atomically $ writeTChan chan cmd

parseCommand :: [String] -> [Command] -> Either [String] Command
parseCommand [] _ = Left ["Error: no command entered\n"]
parseCommand (x:xs) commands = case command of
                        Just (str, toks) -> case expectTokens toks xs of
                                   Left errs -> Left (errs ++ ["in command " ++ x])
                                   Right toks -> Right (x, toks)
                        Nothing -> Left ["Error: invalid command: " ++ x]
    where command = L.find (\(cmd,_) -> cmd == x ) commands

awaitCommands :: Handle -> [Command] -> TChan Command -> TChan [String] -> IO()
awaitCommands handle options cmdChan errChan = do
  input <- words <$> hGetLine handle
  let res = parseCommand input options
  case res of
    Left err -> atomically $ writeTChan errChan err
    Right cmd -> atomically $ writeTChan cmdChan cmd
  awaitCommands handle options cmdChan errChan

runPrinter :: Handle -> TChan [String] -> IO()
runPrinter h chan = do
  errs <- atomically $ readTChan chan
  mapM_ (hPutStrLn h) errs
  runPrinter h chan
