module Panglossian.Controller (awaitConns) where

import Control.Concurrent
import Data.Int
import Network
import System.IO

awaitConns :: Int32  -> IO ()
awaitConns port = do
  sock <- listenOn $ PortNumber $ fromIntegral port
  handleConns sock
     return ()

handleConns :: Socket -> IO ()
handleConns sock = do
  (handle, _, _) <- accept sock
  hSetBuffering handle NoBuffering
  forkIO $ handleConn handle
  handleConns sock

handleConn :: Handle -> IO ()
handleConn conn = return ()
