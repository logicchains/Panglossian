{-# LANGUAGE ScopedTypeVariables #-}
module Panglossian.Controller (awaitConns) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Int
import Data.Maybe
import Data.Word
import Data.Serialize
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

data Command = WantRegionID (TChan Command) | GiveRegionID Word32
             deriving Show

instance Show (TChan a) where
    show a = "TChan "++show a++" "

printC :: TChan [String] -> [String] -> IO ()
printC chan str = atomically $ writeTChan chan str

awaitAny :: [TChan a] -> IO ()
awaitAny chans = atomically $ do
  fullChans <- catMaybes <$> mapM checkFull chans
  unless (null fullChans) retry

manageCons :: [TChan Command] -> TChan (TChan Command) -> TChan Command -> TChan [String] -> IO ()
manageCons cons conChan cmdChan printer = do
  atomically (do
              regionRequests <- catMaybes <$> mapM checkFull cons
              noCmd <- isEmptyTChan cmdChan
              noNewChan <- isEmptyTChan conChan
              when (null regionRequests && noCmd && noNewChan) retry)
  regionRequests <- fmap catMaybes . atomically $ mapM checkFull cons
  newCmd <- atomically $ tryReadTChan cmdChan
  newChan <- atomically $ tryReadTChan conChan
  case newCmd of 
    Nothing -> return ()
    Just cmd -> case cmd of
                  WantRegionID chan -> atomically $ writeTChan chan $ GiveRegionID 1
                  _ -> return ()
  unless (null regionRequests) . atomically $ (catMaybes <$> mapM handleRegionRequest regionRequests) >>= 
             mapM_ (writeTChan cmdChan)
  case newChan of
    Nothing -> manageCons cons conChan cmdChan printer
    Just chan -> atomically (readTChan conChan) >>= (\x -> manageCons (x : cons) conChan cmdChan printer) 

checkFull :: TChan a -> STM (Maybe (TChan a))
checkFull chan = do
  empty <- isEmptyTChan chan
  return (if empty then Nothing else Just chan)

handleRegionRequest :: TChan Command -> STM(Maybe Command)
handleRegionRequest chan = do
  request <- readTChan chan
  case request of
    WantRegionID c -> return . Just $ WantRegionID c
    a -> unGetTChan chan a >> return Nothing

awaitConns :: Int32 -> TChan [String] -> TChan PT.TID -> TChan (TChan Command) -> IO ()
awaitConns port printer tids cmds = do
  printC printer ["Attempting to listening on channel: " ++ show port]
  myId <- myThreadId
  atomically $ writeTChan tids ("Listener on port "++show port++"",myId)
  serve HostAny (show port) (\(sock, sockaddr) -> do 
    printC printer ["Accepting connection on channel: " ++ show port]                                                  
    newCmdChan <- newTChanIO
    atomically $ writeTChan cmds newCmdChan
    handleConn sock sockaddr printer newCmdChan tids)
  return ()

handleConn :: Socket -> SockAddr ->  TChan [String] -> TChan Command -> TChan PT.TID -> IO ()
handleConn sock addr printer cmds tids = do
  printC printer ["Handling connection from address: " ++ show addr]
  atomically $ writeTChan cmds $ WantRegionID cmds
  atomically (readTChan cmds >>= (\x -> case x of
                                             GiveRegionID n -> return n
                                             a -> retry)) >>=
                                    (send sock . encode)>> recv sock 4 >>= tryDecode
  where tryDecode s = case s of
                        Nothing -> printC printer ["Error receiving ID from region: " ++ show addr]
                        Just n -> case decode n of
                                    Left err -> printC printer ["Error decoding ID from region: " ++ show addr ++ "error was" ++ err]
                                    Right n -> myThreadId >>= (\x -> atomically $ writeTChan tids ("Region "++show n++" host",x)) >> 
                                                           handleRegion sock addr printer cmds n

handleRegion :: Socket -> SockAddr -> TChan [String] -> TChan Command -> Word32 -> IO ()
handleRegion sock addr printer cmds id =
  handleRegion sock addr printer cmds id

commands = [("listen",[PC.NumToken 0]), ("exit", []), ("showThreads", [])]

handleCommands :: TChan PC.Command -> TChan PT.Command -> TChan [String] -> TChan PT.TID -> TChan (TChan Command) ->
               TChan Command-> IO()
handleCommands parsedCmdChan sprvsrChan printer tidChan regionsChanChan regionMgrCmdChan   = do
  (cmd, args) <- atomically $ readTChan parsedCmdChan
  case cmd of
    "exit" -> killAll
    "listen" -> case head args of
                  PC.NumToken n -> forkIO (awaitConns n printer tidChan regionsChanChan) >> continue
                  _ -> printC printer ["Fatal internal error: incorrect listen command"] >> killAll
    "showThreads" -> atomically (writeTChan sprvsrChan PT.PrintAll) >> continue
    _ -> continue
 where killAll = atomically $ writeTChan sprvsrChan PT.Kill
       continue = handleCommands parsedCmdChan sprvsrChan printer tidChan regionsChanChan regionMgrCmdChan

runController :: IO ()
runController = do
  printChan <- newTChanIO
  parsedCmdChan <- newTChanIO
  supervisorTidChan <- newTChanIO
  supervisorCmdChan <- newTChanIO
  regionsChanChan <- newTChanIO
  regionMgrCmdChan <- newTChanIO
  printerID <- forkIO $ PC.runPrinter stdout printChan
  cmdProcessorID <- forkIO $ PC.awaitCommands stdin commands parsedCmdChan printChan
  supervisorId <- forkIO $ PT.watchThreads [] supervisorTidChan supervisorCmdChan printChan
  conManagerId <- forkIO  $ manageCons []regionsChanChan regionMgrCmdChan printChan
  cmdHandlerId <- myThreadId                  
  -- Supervisor must be sent first
  atomically $ mapM_ (writeTChan supervisorTidChan) [("Thread supervisor", supervisorId), ("Printer", printerID),
                    ("Input command processor", cmdProcessorID), ("Connection manager", conManagerId),
                    ("Command handler", cmdHandlerId)]
  handleCommands parsedCmdChan supervisorCmdChan printChan supervisorTidChan regionsChanChan regionMgrCmdChan
