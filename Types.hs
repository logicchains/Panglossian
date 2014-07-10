{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Panglossian.Types (ActionT(..), Property(..), Modifier(..), Script(..)) where

import Data.Int
import Data.Text
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Deriving as DU
import qualified Data.Vector as V
import Data.Word
import Prelude hiding (exponent)

data Modifier =  Modifier {
	modifiesProp :: Word32,
	exponent :: Word8,
	multiplier :: Word32
    } deriving Show

DU.derivingUnbox "Modifier"
 [t| Modifier -> (Word32, Word8, Word32)|]
 [| \Modifier{modifiesProp,exponent,multiplier} -> (modifiesProp,exponent,multiplier)|]
 [| \(modifiesProp,exponent,multiplier) -> Modifier{modifiesProp,exponent,multiplier}|]

data Property = Property {
	propertyID :: Word32,
	value :: Int64
    } deriving Show

DU.derivingUnbox "Property"
 [t| Property -> (Word32, Int64)|]
 [| \Property{propertyID,value} -> (propertyID, value)|]
 [| \(propertyID,value) -> Property{propertyID,value}|]

type Script =  Word32
          
data ActionT = ActionT {
      actName :: Text,
      constraints :: V.Vector Script,
      specials ::   V.Vector Script,
      actorAffects :: U.Vector Modifier,
      targetAffects :: U.Vector Modifier,
      prereqs :: U.Vector Property,
      consumes :: U.Vector Property
    } deriving Show

data Object = Object {
      objID :: Word32,
      properties :: U.Vector Property
    } deriving Show
