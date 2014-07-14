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
              newCmd <- isEmptyTChan cmdChan
              newChan <- isEmptyTChan conChan
              when (null regionRequests && not newCmd && not newChan) retry)
  regionRequests <- fmap catMaybes . atomically $ mapM checkFull cons
  newCmd <- atomically $ isEmptyTChan cmdChan
  newChan <- atomically $ isEmptyTChan conChan
  when newCmd $ atomically (readTChan cmdChan) >>= 
           (\x -> case x of
                    WantRegionID chan -> atomically $ writeTChan chan $ GiveRegionID 1
                    _ -> return ()
           )
  unless (null regionRequests) . atomically $ (catMaybes <$> mapM handleRegionRequest regionRequests) >>= 
             mapM_ (writeTChan cmdChan)
  if newChan then atomically (readTChan conChan) >>= (\x -> manageCons (x : cons) conChan cmdChan printer) 
  else manageCons cons conChan cmdChan printer

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

awaitConns :: Int32 -> TChan [String] -> TChan ThreadId -> TChan (TChan Command) -> IO ()
awaitConns port printer tids cmds = do
  printC printer ["Attempting to listening on channel: " ++ show port]
  serve HostAny (show port) (\(sock, sockaddr) -> do 
    printC printer ["Accepting connection on channel: " ++ show port]                                                  
    newCmdChan <- newTChanIO
    atomically $ writeTChan cmds newCmdChan
    handleConn sock sockaddr printer newCmdChan)
  return ()

handleConn :: Socket -> SockAddr ->  TChan [String] -> TChan Command -> IO ()
handleConn sock addr printer cmds = do
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
                                    Right n -> handleRegion sock addr printer cmds n

handleRegion :: Socket -> SockAddr -> TChan [String] -> TChan Command -> Word32 -> IO ()
handleRegion sock addr printer cmds id =
  handleRegion sock addr printer cmds id

commands = [("listen",[PC.NumToken 0]), ("exit", [])]
handleCommands :: TChan PC.Command -> TChan PT.Command -> TChan [String] -> TChan ThreadId -> TChan (TChan Command) ->
               TChan Command-> IO()
handleCommands parsedCmdChan sprvsrChan printer tidChan regionsChanChan regionMgrCmdChan   = do
  (cmd, args) <- atomically $ readTChan parsedCmdChan
  case cmd of
    "exit" -> killAll
    "listen" -> case head args of
                  PC.NumToken n -> forkIO (awaitConns n printer tidChan regionsChanChan) >> continue
                  _ -> printC printer ["Fatal internal error: incorrect listen command"] >> killAll
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
  cmdHndlrID <- forkIO $ PC.awaitCommands stdin commands parsedCmdChan printChan
  supervisorId <- forkIO $ PT.watchThreads [] supervisorTidChan supervisorCmdChan
  conManagerId <- forkIO  $ manageCons []regionsChanChan regionMgrCmdChan printChan
  atomically $ mapM (writeTChan supervisorTidChan) [supervisorId, printerID, cmdHndlrID] -- Supervisor must be sent first
  handleCommands parsedCmdChan supervisorCmdChan printChan supervisorTidChan regionsChanChan regionMgrCmdChan
