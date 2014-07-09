{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
module Panglossian.Loader (findFilesSuffixed) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Either.Unwrap (fromLeft, fromRight, isRight)
import qualified Data.Set as S
import Data.Int
import qualified Data.List as L
import Data.Text
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word
import Debug.Trace
import qualified System.FilePath.FilePather as Pather
import qualified Data.ByteString.Lazy as BS

import Panglossian.Types as P

actionSuffix = "_A.json"
actionFolder = "./Panglossian"
scriptSuffix = "_S.json"
scriptFolder = "./Panglossian"

type NameList = ([Text], S.Set Text)

data JAction = JAction {
      name :: Text,
      constraints ::  [Text],
      specials ::  [Text],
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
	modifiedBy :: Text,
	exponent :: Word8,
	multiplier :: Word32
    } deriving Show

instance FromJSON JModifier where
    parseJSON (Object v) = JModifier <$> v .: "modifiedBy" <*> v .: "exponent" <*> v .: "multiplier"
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

type NamePairs = (NameList, NameList)

parseActions :: NamePairs -> [JAction] -> ([P.Action], NamePairs) -> ([P.Action], NamePairs)
parseActions  _ [] res = res
parseActions names (x:xs) (js,_) = parseActions newNames xs ((j:js),newNames)
    where (j, newNames) = parseAction names x 


parseAction :: NamePairs -> JAction ->  (P.Action, NamePairs)
parseAction (propNames, scriptNames) JAction {..} = (P.Action {name, constraints=newConstraints, specials=newSpecials,
                                                actorAffects=newAAffects, targetAffects=newTAffects, 
                                               prereqs=newPrereqs, consumes=newConsumes}, (newPNames4, newSNames2))
    where 
      (newPrereqs, newPNames) = parseJTypes parseProperty propNames prereqs ([],([],S.empty))
      (newConsumes, newPNames2) = parseJTypes parseProperty newPNames consumes ([],([],S.empty))
      (newAAffects, newPNames3) = parseJTypes parseModifier newPNames2 actorAffects ([],([],S.empty))
      (newTAffects, newPNames4) = parseJTypes parseModifier newPNames3 targetAffects ([],([],S.empty))
      (newConstraints, newSNames) = parseJTypes parseScriptName scriptNames constraints ([],([],S.empty))
      (newSpecials, newSNames2) = parseJTypes parseScriptName newSNames specials ([],([],S.empty))

type JParser a b = (NameList -> a -> (b, NameList))

parseJTypes :: JParser ja a ->  NameList -> [ja] -> ([a], NameList) -> ([a], NameList)
parseJTypes _ names [] (res,_) = (res, names)
parseJTypes parseFunc names (x:xs) (js,_) = parseJTypes parseFunc newNames xs ((j:js),names)
    where (j, newNames) = parseFunc names x 

parseScriptName :: NameList -> Text -> (P.Script, NameList)
parseScriptName names name  = (P.Script {scriptID = fromIntegral index, body= "NULL"}, newNames)
    where (index, newNames) =  getPropNameIndex names name

parseModifier :: NameList -> JModifier ->  (P.Modifier, NameList)
parseModifier names JModifier{..}  = (P.Modifier {modifiesProp = fromIntegral index, exponent, multiplier}, newNames)
    where (index, newNames) =  getPropNameIndex names modifiedBy

parseProperty :: NameList -> JProperty ->  (P.Property, NameList)
parseProperty names JProperty{..}  = (P.Property {propertyID = fromIntegral index, value}, newNames)
    where (index, newNames) =  getPropNameIndex names property

getPropNameIndex :: NameList -> Text -> (Int, NameList)
getPropNameIndex (registry, hash) name = if S.member name hash then (L.length registry, (registry, hash))  
                                      else ((L.length registry) + 1, (registry ++ [name], S.insert name hash))

dbg a = (trace ("\n\ndoing it" ++(show a) ++"\n") a)
          
loadAll :: IO [()]
loadAll = do
  actionFiles <- findFilesSuffixed actionSuffix actionFolder
  scriptFiles <- findFilesSuffixed scriptSuffix scriptFolder
  (jActs, actErrs) <- (loadJType actionFiles :: IO ([JAction], [String]))
  (jScripts, scriptErrs) <- (loadJType scriptFiles :: IO ([JScript], [String]))
  let parseResults = parseActions (([], S.empty), ([], S.empty)) jActs ([], (([], S.empty), ([], S.empty)))
  let (actions, ((propNames,propHash), (scriptNames,scriptHash))) = parseResults
  mapM print actions
  mapM print scriptNames
  mapM print propNames
