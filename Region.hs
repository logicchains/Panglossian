module Panglossian.Region (runRegion) where

import Control.Concurrent
import Control.Concurrent.STM
import System.IO

import qualified Panglossian.CmdParser as PC
import qualified Panglossian.ThreadCtrl as PT

commands = [("listen",[PC.NumToken 0]), ("exit", [])]

runRegion :: IO()
runRegion = do
  printChan <- newTChanIO
  parsedCmdChan <- newTChanIO
  supervisorTidChan <- newTChanIO
  supervisorCmdChan <- newTChanIO
  clientChanChan <- newTChanIO
  clientMgrCmdChan <- newTChanIO
  printerID <- forkIO $ PC.runPrinter stdout printChan
  cmdProcessorID <- forkIO $ PC.awaitCommands stdin commands parsedCmdChan printChan
  supervisorId <- forkIO $ PT.watchThreads [] supervisorTidChan supervisorCmdChan printChan
--  clientManagerId <- forkIO  $ manageClients []clientChanChan clientMgrCmdChan printChan
  cmdHandlerId <- myThreadId
  -- Supervisor must be sent first
  atomically $ mapM_ (writeTChan supervisorTidChan) [("Thread supervisor", supervisorId), ("Printer", printerID),
                    ("Input command processor", cmdProcessorID),-- ("Connection manager", clientManagerId),
                    ("Command handler", cmdHandlerId)]
--  handleCommands parsedCmdChan supervisorCmdChan printChan supervisorTidChan regionsChanChan regionMgrCmdChan

