{-# LANGUAGE ScopedTypeVariables #-}
module Panglossian.Controller (awaitConns) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Int
--import Network
import Network.Simple.TCP
import System.IO

import qualified Panglossian.CmdParser as PC
import qualified Panglossian.ThreadCtrl as PT

{-
awaitConns2 :: Int32 -> TChan [String] -> TChan ThreadId -> IO ()
awaitConns2 port printer tids = do
  atomically $ writeTChan printer ["Listening on channel: " ++ show port]
  lisRes <- try doListen 
  case lisRes of
    Left (ex :: SomeException) -> atomically $ writeTChan printer 
                                ["Warning: couldn't listen on port: " ++ (show port) ++ " due to exception " ++ show ex]
    Right _ -> return ()
  where doListen = do
          sock <- listenOn $ PortNumber $ fromIntegral port
          handleConns sock tids
          return ()

handleConns :: Socket -> TChan ThreadId -> IO ()
handleConns sock tids = do
  (handle, _, _) <- accept sock
  hSetBuffering handle NoBuffering
  conID <- forkIO $ handleConn handle
  atomically $ writeTChan tids conID
  handleConns sock tids

-}
printC :: TChan [String] -> [String] -> IO ()
printC chan str = atomically $ writeTChan chan str

awaitConns :: Int32 -> TChan [String] -> TChan ThreadId -> IO ()
awaitConns port printer tids = do
  printC printer ["Attempting to listening on channel: " ++ show port]
  serve HostAny (show port) (\(sock, sockaddr) -> do 
    printC printer ["Accepting connection on channel: " ++ show port]                                                  
    handleConn sock printer)
  return ()

handleConn :: Socket -> TChan [String] -> IO ()
handleConn sock printer = 
    return ()

commands = [("listen",[PC.NumToken 0]), ("exit", [])]

handleCommands :: TChan PC.Command -> TChan PT.Command -> TChan [String] -> TChan ThreadId -> IO()
handleCommands cmdChan sprvsrChan printer tidChan  = do
  (cmd, args) <- atomically $ readTChan cmdChan
  case cmd of
    "exit" -> killAll
    "listen" -> case (args !! 0) of
                  PC.NumToken n -> (forkIO $ awaitConns n printer tidChan) >> continue
                  _ -> (printC printer ["Fatal internal error: incorrect listen command"]) >> killAll
    _ -> continue
 where killAll = atomically $ writeTChan sprvsrChan PT.Kill
       continue = handleCommands cmdChan sprvsrChan printer tidChan

runController :: IO ()
runController = do
  printChan <- newTChanIO
  cmdChan <- newTChanIO
  supervisorTidChan <- newTChanIO
  supervisorCmdChan <- newTChanIO
  printerID <- forkIO $ PC.runPrinter stdout printChan
  cmdHndlrID <- forkIO $ PC.awaitCommands stdin commands cmdChan printChan
  supervisorId <- forkIO $ PT.watchThreads [] supervisorTidChan supervisorCmdChan
  atomically $ mapM (writeTChan supervisorTidChan) [supervisorId, printerID, cmdHndlrID] -- Supervisor must be sent first
  handleCommands cmdChan supervisorCmdChan printChan supervisorTidChan
