module Panglossian.ThreadCtrl (watchThreads, Command(..), TID) where

import Control.Concurrent
import GHC.Conc
import Control.Concurrent.STM
import Control.Monad

data Command = Kill | PrintAll

type TID = (String, ThreadId)

printC :: TChan [String] -> [String] -> IO ()
printC chan str = atomically $ writeTChan chan str

watchThreads :: [TID] -> TChan TID -> TChan Command -> TChan [String] -> IO()
watchThreads tids tidChan cChan printer = do
  atomically (do
               noTid <- isEmptyTChan tidChan 
               noCmd <- isEmptyTChan cChan
               when (noTid && noCmd) retry)
  noTid <- atomically $ isEmptyTChan tidChan 
  noCmd <- atomically $ isEmptyTChan cChan
  unless noCmd (do
      cmd <- atomically $ readTChan cChan
      case cmd of
        Kill -> void (mapM_ (killThread . snd) tids)
        PrintAll -> mapM showTID tids >>= printC printer
        _ -> return ())
  if not noTid then do
      newTid <- atomically $ readTChan tidChan
      watchThreads (newTid:tids) tidChan cChan printer
  else watchThreads tids tidChan cChan printer

showTID :: TID -> IO String
showTID (name,tid) = do
  status <- threadStatus tid
  return $ show tid ++ "(" ++ name ++ "): " ++ show status
