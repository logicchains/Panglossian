module Panglossian.IntermediateTypes (LAction(..)) where

import Data.Text
import Panglossian.Types as P

data LAction = LAction {
      actName :: Text,
      constraints :: [P.Script],
      specials ::  [P.Script],
      actorAffects :: [P.Modifier],
      targetAffects :: [P.Modifier],
      prereqs :: [P.Property],
      consumes :: [P.Property]
    } deriving Show

