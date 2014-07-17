{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns, TemplateHaskell #-}
module Panglossian.Loader (findFilesSuffixed) where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Data.Either
import qualified Data.Set as S
import Data.Int
import qualified Data.List as L
import qualified Data.Map as M
import Data.Text
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word
import qualified System.FilePath.FilePather as Pather
import qualified Data.ByteString.Lazy as BS

import Panglossian.Types as P
import Panglossian.IntermediateTypes as IT

import Debug.Trace

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
findFilesSuffixed suffix = Pather.findp filterPred recursePred
    where 
      filterPred = Pather.filterPredicate' (\x -> L.drop (L.length x - L.length suffix) x == suffix)
      recursePred = Pather.recursePredicate $ const True

loadJType :: FromJSON a => [String] -> IO ([String], [a])
loadJType fileNames = partitionEithers <$> possibleActions
    where
      possibleActions = L.map toAction <$> mapM BS.readFile fileNames
      toAction x = eitherDecode x :: FromJSON a => Either String a

compileJObj :: M.Map Text Word32 -> JObject -> (Text, [P.Property])
compileJObj propIds JObject {..} = (objname, L.map (compileProp propIds) objproperties)

compileJAct :: M.Map Text Word32 -> M.Map Text Word32  -> JAction -> IT.LAction
compileJAct propIds scriptIds JAction {..} = 
    IT.LAction actname (L.map (scriptIds M.!) actconstraints) (L.map (scriptIds M.!) actspecials) 
      newAAffects newTAffects newPrereqs newConsumes
    where 
          newPrereqs = L.map (compileProp propIds) actprereqs
          newConsumes = L.map (compileProp propIds) actconsumes
          newAAffects = L.map (compileMod propIds) actactorAffects
          newTAffects = L.map (compileMod propIds) acttargetAffects
          
compileMod :: M.Map Text Word32 -> JModifier -> P.Modifier
compileMod ids JModifier{..} = P.Modifier (ids M.! modifiedBy) exponent multiplier

compileProp :: M.Map Text Word32 -> JProperty -> P.Property
compileProp ids JProperty{..} = P.Property (ids M.! property) value

getObjProps :: JObject -> S.Set Text
getObjProps jObj = getPropNames (objproperties jObj) property

getActProps :: JAction -> S.Set Text
getActProps JAction{..} = S.union (getPropNames (actprereqs ++ actconsumes) property)
                                  (getPropNames (actactorAffects ++ acttargetAffects) Panglossian.Loader.modifiedBy)

getPropNames :: [a] -> (a -> Text) -> S.Set Text
getPropNames jList accessor = S.fromList (L.map accessor jList)

makeIndices :: S.Set Text -> M.Map Text Word32
makeIndices set = M.fromAscList (L.zip (S.toAscList set) [0.. fromIntegral $ S.size set]) 

findNLoadJType :: FromJSON a => String -> String -> IO ([String], [a])
findNLoadJType suffix folder = L.sort <$> findFilesSuffixed suffix folder >>= loadJType
          
loadAll :: IO ()
loadAll = do
  (actErrs, jActs) <- findNLoadJType actionSuffix actionFolder :: IO ([String], [JAction])
  (scriptErrs, jScripts) <- findNLoadJType scriptSuffix scriptFolder :: IO ([String], [JScript])
  (objErrs, jObjs) <- findNLoadJType objectSuffix objectFolder :: IO ([String], [JObject])
  let scriptIndices = makeIndices $ getPropNames jScripts scrname
  let propIndices = makeIndices . S.unions $ L.map getActProps jActs ++ L.map getObjProps jObjs
  let actions = L.map (compileJAct propIndices scriptIndices) jActs
  let objects = L.map (compileJObj propIndices) jObjs
  mapM_ print objects
  print $ show propIndices

finaliseAction :: IT.LAction -> P.ActionT
finaliseAction IT.LAction{..} = P.ActionT{actName, constraints = V.fromList constraints, specials = V.fromList specials,
                                         actorAffects = U.fromList actorAffects, targetAffects = U.fromList targetAffects,
                                          prereqs = U.fromList prereqs, consumes = U.fromList consumes}
