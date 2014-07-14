module Panglossian.ThreadCtrl (watchThreads, Command(..)) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

data Command = Kill

watchThreads :: [ThreadId] -> TChan ThreadId -> TChan Command -> IO()
watchThreads tids tidChan cChan = do
  atomically (do
               noTid <- isEmptyTChan tidChan 
               noCmd <- isEmptyTChan cChan
               when (noTid && noCmd) retry)
  noTid <- atomically $ isEmptyTChan tidChan 
  noCmd <- atomically $ isEmptyTChan cChan
  unless noCmd (do
      cmd <- atomically $ readTChan cChan
      case cmd of
        Kill -> void (mapM_ killThread tids)
        _ -> return ())
  if not noTid then do
      newTid <- atomically $ readTChan tidChan
      watchThreads (newTid:tids) tidChan cChan
  else watchThreads tids tidChan cChan
