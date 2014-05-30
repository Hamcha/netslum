{-# LANGUAGE OverloadedStrings #-}

module Server
( createServer
) where

import           Control.Concurrent
import           Control.Monad
import           Network
import qualified Network.Socket as NS
import           Network.Socket.ByteString

handle :: Socket -> IO ()
handle client = do
    sendAll client "POOF!"
    sClose client

acceptAll :: Socket -> IO ()
acceptAll sock = do
    (client, _) <- NS.accept sock
    forkIO $ handle client
    acceptAll sock

createServer :: PortNumber -> IO ()
createServer port = withSocketsDo $ do
    -- Create socket and accept everyone
    sock <- listenOn $ PortNumber port
    acceptAll sock
    sClose sock