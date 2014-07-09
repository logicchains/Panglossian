{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Panglossian.Loader (findFilesSuffixed) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Either.Unwrap (fromLeft, fromRight, isRight)
import Data.HashSet
import Data.Int
import qualified Data.List as L
import Data.Text
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word
import qualified System.FilePath.FilePather as Pather
import qualified Data.ByteString.Lazy as BS

import Panglossian.Types as P

actionSuffix = "_A.json"
actionFolder = "./Panglossian"
scriptSuffix = "_S.json"
scriptFolder = "./Panglossian"

type NameList = ([Text], HashSet Text)

data JAction = JAction {
      name :: Text,
      constraints ::  [JScript],
      specials ::  [JScript],
      actorAffects :: [JModifier],
      targetAffects :: [JModifier],
      prereqs :: [JProperty],
      consumes :: [JProperty]
    } deriving Show

instance FromJSON JAction where
     parseJSON (Object v) = JAction <$>
                            v .: "name" <*>
                            v .: "constraints" <*>
                            v .: "specials" <*>
                            v .: "actorAffects" <*>
                            v .: "targetAffects" <*>
                            v .: "prereqs" <*>
                            v .: "consumes"
     parseJSON _          = mzero

data JModifier =  JModifier {
	modifiesProp :: Text,
	exponent :: Word8,
	multiplier :: Word32
    } deriving Show

instance FromJSON JModifier where
    parseJSON (Object v) = JModifier <$> v .: "modifiesProp" <*> v .: "exponent" <*> v .: "multiplier"
    parseJSON _          = mzero

data JProperty = JProperty {
	property :: Text,
	value :: Int64
    } deriving Show

instance FromJSON JProperty where
    parseJSON (Object v) = JProperty <$> v .: "property" <*> v .: "value"
    parseJSON _          = mzero

data JScript = JScript {
	scriptName :: Text,
	body :: Text
    } deriving Show

instance FromJSON JScript where
    parseJSON (Object v) = JScript <$> v .: "name" <*> v .: "body"
    parseJSON _          = mzero

findFilesSuffixed :: String -> String -> IO [FilePath]
findFilesSuffixed suffix folderName = Pather.findp filterPred recursePred folderName
    where 
      filterPred = Pather.filterPredicate' (\x -> (L.drop (L.length x - (L.length suffix)) x) == suffix)
      recursePred = Pather.recursePredicate (\x -> True)

loadJType :: FromJSON a => [String] -> IO ([a], [String])
loadJType fileNames = do
  acts <- (L.map fromRight) <$> fst <$> parts
  errs <- (L.map fromLeft) <$> snd <$> parts
  return (acts, errs)
    where
      parts = fmap (L.partition isRight) possibleActions
      possibleActions = fmap (L.map toAction) $ mapM BS.readFile fileNames
      toAction = (\x -> eitherDecode x :: FromJSON a => Either String a)

parseAction :: NameList-> NameList ->  JAction ->  (P.Action, NameList, NameList)
parseAction propNames scriptNames JAction {..} = (P.Action {name, constraints=newConstraints, specials=newSpecials,
                                                actorAffects=newAAffects, targetAffects=newTAffects, 
                                               prereqs=newPrereqs, consumes=newConsumes}, newPNames4, newSNames2)
    where 
      (newPrereqs, newPNames) = parseJTypes parseProperty propNames prereqs ([],([],Data.HashSet.empty))
      (newConsumes, newPNames2) = parseJTypes parseProperty newPNames consumes ([],([],Data.HashSet.empty))
      (newAAffects, newPNames3) = parseJTypes parseModifier newPNames2 actorAffects ([],([],Data.HashSet.empty))
      (newTAffects, newPNames4) = parseJTypes parseModifier newPNames3 targetAffects ([],([],Data.HashSet.empty))
      (newConstraints, newSNames) = parseJTypes parseScript scriptNames constraints ([],([],Data.HashSet.empty))
      (newSpecials, newSNames2) = parseJTypes parseScript newSNames specials ([],([],Data.HashSet.empty))


type JParser a b = (NameList -> a -> (b, NameList))

parseJTypes :: JParser ja a ->  NameList -> [ja] -> ([a], NameList) -> ([a], NameList)
parseJTypes _  _ [] res = res
parseJTypes parseFunc names (x:xs) (js,_) = parseJTypes parseFunc newNames xs ((j:js),newNames)
    where (j, newNames) = parseFunc names x 

parseScript :: NameList -> JScript -> (P.Script, NameList)
parseScript names JScript{..}  = (P.Script {scriptID = fromIntegral index, body}, newNames)
    where (index, newNames) =  getPropNameIndex names scriptName

parseModifier :: NameList -> JModifier ->  (P.Modifier, NameList)
parseModifier names JModifier{..}  = (P.Modifier {modifiesProp = fromIntegral index, exponent, multiplier}, newNames)
    where (index, newNames) =  getPropNameIndex names modifiesProp

parseProperty :: NameList -> JProperty ->  (P.Property, NameList)
parseProperty names JProperty{..}  = (P.Property {propertyID = fromIntegral index, value}, newNames)
    where (index, newNames) =  getPropNameIndex names property

getPropNameIndex :: NameList -> Text -> (Int, NameList)
getPropNameIndex (registry, hash) name = if member name hash then (L.length registry, (registry, hash))  
                                      else ((L.length registry) + 1, (registry ++ [name], insert name hash))
          
loadAll :: IO [()]
loadAll = do
  actionFiles <- findFilesSuffixed actionSuffix actionFolder
  scriptFiles <- findFilesSuffixed scriptSuffix scriptFolder
  (jActs, actErrs) <- (loadJType actionFiles :: IO ([JAction], [String]))
  (jScripts, scriptErrs) <- (loadJType scriptFiles :: IO ([JScript], [String]))
  mapM print jActs
  mapM putStrLn actErrs
