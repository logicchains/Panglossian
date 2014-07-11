module Panglossian.CmdParser (parseCommand) where

import Control.Concurrent.STM
import Data.Int

data Token = NumToken Int32 | StrToken String

type Command = (String, [Token])

parseInt :: String -> Maybe Int32
parseInt str = case (reads str :: [(Int32, String)]) of
    [] -> Nothing
    [(num, _)] -> Just num

expectToken :: Token -> String -> Either String Token
expectToken (StrToken _) str = StrToken str
expectToken (NumToken _) str = case (parseInt str) of
                                  Just n -> NumToken n
                                  Nothing -> "Error: "++str++" is not an int."

expectTokens :: [Token] -> [String] -> Maybe [Token]  
expectTokens expected actual = zipWith expectToken expected actual

parseCommand :: [String] -> Command
parseCommand str = ("", [NumToken 10])
