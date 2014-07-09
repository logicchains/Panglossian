module Panglossian.Types (Action, Modifier) where

import Data.Int
import Data.Text
import Data.Word

data Action = Action {
      name :: Text,
      constraints :: [Script],
      specials ::  [Script],
      actorAffects :: [Modifier],
      targetAffects :: [Modifier],
      prereqs :: [Property],
      consumes :: [Property]
    } deriving Show

data Modifier =  Modifier {
	modifiesProp :: Word32,
	exponent :: Word8,
	multiplier :: Word32
    } deriving Show

data Property = Property {
	property :: Word32,
	value :: Int64
    } deriving Show

data Script = Script Text deriving Show
