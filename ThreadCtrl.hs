module Panglossian.ThreadCtrl (watchThreads, Command(..)) where

import Control.Concurrent
import Control.Concurrent.STM

data Command = Kill

watchThreads :: [ThreadId] -> TChan ThreadId -> TChan Command -> IO()
watchThreads tids tidChan cChan = do
  noTid <- atomically $ isEmptyTChan tidChan 
  noCmd <- atomically $ isEmptyTChan cChan
  if not noTid then do
      newTid <- atomically $ readTChan tidChan
      watchThreads (newTid:tids) tidChan cChan
  else if not noCmd then do
      cmd <- atomically $ readTChan cChan
      case cmd of
        Kill -> mapM killThread tids >> return ()
        _ -> watchThreads tids tidChan cChan
  else watchThreads tids tidChan cChan


--  case cmd of
--    "exit" -> killThread
--    _ -> handleCommands chan