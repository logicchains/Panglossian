{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
module Panglossian.Loader (findFilesSuffixed) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Char as Ch
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
objectSuffix = "_O.json"
objectFolder = "./Panglossian"

data JProperty = JProperty {
	property :: Text,
	value :: Int64
    } deriving Show
$(deriveFromJSON defaultOptions ''JProperty)

data JModifier =  JModifier {
	modifiedBy :: Text,
	exponent :: Word8,
	multiplier :: Word32
    } deriving Show
$(deriveFromJSON defaultOptions ''JModifier)

data JAction = JAction {
      actname :: Text,
      actconstraints ::  [Text],
      actspecials ::  [Text],
      actactorAffects :: [JModifier],
      acttargetAffects :: [JModifier],
      actprereqs :: [JProperty],
      actconsumes :: [JProperty]
    } deriving Show
$(deriveFromJSON defaultOptions{fieldLabelModifier =  L.drop 3} ''JAction)

data JObject = JObject{
      objname :: Text,
      objproperties :: [JProperty]
    } deriving Show
$(deriveFromJSON defaultOptions{fieldLabelModifier =  L.drop 3} ''JObject)

data JScript = JScript {
	scrname :: Text,
	scrbody :: Text
    } deriving Show
$(deriveFromJSON defaultOptions{fieldLabelModifier =  L.drop 3} ''JScript)

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
      possibleActions = L.map toAction <$> mapM BS.readFile fileNames
      toAction = (\x -> eitherDecode x :: FromJSON a => Either String a)

type NameList = ([Text], S.Set Text)
type NamePairs = (NameList, NameList)

parseAction :: NamePairs -> JAction ->  (NamePairs, IT.LAction)
parseAction (propNames, scriptNames) JAction {..} = ((newPNames4, newSNames2), IT.LAction {actName=actname, 
                           constraints=newConstraints,specials=newSpecials, actorAffects=newAAffects, 
                           targetAffects=newTAffects, prereqs=newPrereqs, consumes=newConsumes})
    where 
      (newPNames, newPrereqs) = L.mapAccumL parseProperty propNames actprereqs
      (newPNames2, newConsumes) = L.mapAccumL parseProperty newPNames actconsumes
      (newPNames3, newAAffects) = L.mapAccumL parseModifier newPNames2 actactorAffects
      (newPNames4,newTAffects) = L.mapAccumL parseModifier newPNames3 acttargetAffects
      (newSNames, newConstraints) = L.mapAccumL parseScriptName scriptNames actconstraints
      (newSNames2, newSpecials) = L.mapAccumL parseScriptName newSNames actspecials

parseScriptName :: NameList -> Text -> (NameList, P.Script)
parseScriptName names name  = (newNames, fromIntegral index)
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

findNLoadJType :: FromJSON a => String -> String -> IO ([a], [String])
findNLoadJType suffix folder = L.sortBy compare <$> findFilesSuffixed suffix folder >>= loadJType
          
loadAll :: IO [()]
loadAll = do
  (jActs, actErrs) <- findNLoadJType actionSuffix actionFolder :: IO ([JAction], [String])
  (jScripts, scriptErrs) <- findNLoadJType scriptSuffix scriptFolder :: IO ([JScript], [String])
  (jObjs, objErrs) <- findNLoadJType objectSuffix objectFolder :: IO ([JScript], [String])
  let parseResults = L.mapAccumL parseAction (([], S.empty), ([], S.empty)) jActs 
  let (((propNames,propHash), (scriptNames,scriptHash)), actions) = parseResults
  mapM print actions
  mapM print jObjs
  mapM print scriptNames
  mapM print propNames
  mapM print actErrs
