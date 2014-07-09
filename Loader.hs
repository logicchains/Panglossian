{-# LANGUAGE OverloadedStrings #-}
module Panglossian.Loader (findFilesSuffixed) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Either.Unwrap (fromLeft, fromRight, isRight)
import Data.Int
import qualified Data.List as L
import Data.Text
import qualified Data.Vector as V
import Data.Word
import qualified System.FilePath.FilePather as Pather
import qualified Data.ByteString.Lazy as BS

actionSuffix = "_A.json"
actionFolder = "./Panglossian"

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



loadAll :: IO [()]
loadAll = do
  actionFiles <- findFilesSuffixed actionSuffix actionFolder
  (jActs, errs) <- (loadJType actionFiles :: IO ([JAction], [String]))
  mapM putStrLn errs

