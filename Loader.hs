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
import Data.Ord
import Data.Word
import Debug.Trace
import qualified System.FilePath.FilePather as Pather
import System.FilePath
import qualified Data.ByteString.Lazy as BS

import Panglossian.Types as P
import Panglossian.IntermediateTypes as IT

actionSuffix = "_A.json"
actionFolder = "./Panglossian"
scriptSuffix = "_S.json"
scriptFolder = "./Panglossian"

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

sortFilePaths :: [FilePath] -> [FilePath]
sortFilePaths paths = L.sortBy sortFiles paths
    where sortFiles = (\a b -> if (takeFileName a) < (takeFileName b) then LT
                               else if (takeFileName a) > (takeFileName b) then GT
                               else EQ)

loadJType :: FromJSON a => [String] -> IO ([a], [String])
loadJType fileNames = do
  acts <- (L.map fromRight) <$> fst <$> parts
  errs <- (L.map fromLeft) <$> snd <$> parts
  return (acts, errs)
    where
      parts = fmap (L.partition isRight) possibleActions
      possibleActions = fmap (L.map toAction) $ mapM BS.readFile fileNames
      toAction = (\x -> eitherDecode x :: FromJSON a => Either String a)

type NameList = ([Text], S.Set Text)
type NamePairs = (NameList, NameList)

parseActions :: NamePairs -> [JAction] -> ([IT.LAction], NamePairs) -> ([IT.LAction], NamePairs)
parseActions  _ [] res = res
parseActions names (x:xs) (js,_) = parseActions newNames xs ((j:js),newNames)
    where (j, newNames) = parseAction names x 

parseAction :: NamePairs -> JAction ->  (IT.LAction, NamePairs)
parseAction (propNames, scriptNames) JAction {..} = (IT.LAction {name, constraints=newConstraints, specials=newSpecials,
                                                actorAffects=newAAffects, targetAffects=newTAffects, 
                                               prereqs=newPrereqs, consumes=newConsumes}, (newPNames4, newSNames2))
    where 
      (newPNames, newPrereqs) = L.mapAccumL parseProperty propNames prereqs
      (newPNames2, newConsumes) = L.mapAccumL parseProperty newPNames consumes
      (newPNames3, newAAffects) = L.mapAccumL parseModifier newPNames2 actorAffects
      (newPNames4,newTAffects) = L.mapAccumL parseModifier newPNames3 targetAffects
      (newSNames, newConstraints) = L.mapAccumL parseScriptName scriptNames constraints
      (newSNames2, newSpecials) = L.mapAccumL parseScriptName newSNames specials

parseScriptName :: NameList -> Text -> (NameList, P.Script)
parseScriptName names name  = (newNames, P.Script {scriptID = fromIntegral index, body= "NULL"})
    where (index, newNames) =  getPropNameIndex names name

parseModifier :: NameList -> JModifier -> (NameList, P.Modifier)
parseModifier names JModifier{..}  = (newNames, P.Modifier {modifiesProp = fromIntegral index, exponent, multiplier})
    where (index, newNames) =  getPropNameIndex names modifiedBy

parseProperty :: NameList -> JProperty -> (NameList, P.Property)
parseProperty names JProperty{..}  = (newNames, P.Property {propertyID = fromIntegral index, value})
    where (index, newNames) =  getPropNameIndex names property

getPropNameIndex :: NameList -> Text -> (Int, NameList)
getPropNameIndex (registry, hash) name = if S.member name hash then (L.length registry, (registry, hash))  
                                      else (L.length registry, (registry ++ [name], S.insert name hash))

dbg a = (trace ("\n\ndoing it" ++(show a) ++"\n") a)
          
loadAll :: IO [()]
loadAll = do
  actionFiles <- sortFilePaths <$> findFilesSuffixed actionSuffix actionFolder
  scriptFiles <-  sortFilePaths <$> findFilesSuffixed scriptSuffix scriptFolder
  (jActs, actErrs) <- (loadJType actionFiles :: IO ([JAction], [String]))
  (jScripts, scriptErrs) <- (loadJType scriptFiles :: IO ([JScript], [String]))
  let parseResults = parseActions (([], S.empty), ([], S.empty)) jActs ([], (([], S.empty), ([], S.empty)))
  let (actions, ((propNames,propHash), (scriptNames,scriptHash))) = parseResults
  mapM print actions
  mapM print scriptNames
  mapM print propNames

--printProperties 
