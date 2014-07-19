module Panglossian.IntermediateTypes (LAction(..)) where

import Data.Text
import Data.Word
import Panglossian.Types as P

data LAction = LAction {
      actName :: Text,
      constraints :: [P.Script],
      specials ::  [P.Script],
      actorAffects :: [P.Modifier],
      targetAffects :: [P.Modifier],
      prereqs :: [P.Property],
      consumes :: [P.Property],
      apCost :: Word16,
      divisible :: Bool
    } deriving Show

