{-# LANGUAGE OverloadedStrings #-}

module Server
( createServer
) where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Char8 as B
import           Network
import qualified Paradise.Client as Paradise
import           System.IO

command :: Handle -> Paradise.PlayerData -> B.ByteString
           -> Paradise.PlayerData
command client pdata line = pdata

handle :: Handle -> Paradise.PlayerData -> Bool -> IO ()
handle client pdata True  = return ()
handle client pdata False = do
    -- Read input and execute it
    line  <- B.hGetLine client
    let pdata = command client pdata line
    -- Continue if there is more data
    hIsEOF client >>= handle client pdata

acceptAll :: Socket -> IO ()
acceptAll sock = do
    (client, _, _) <- accept sock
    hSetBuffering client NoBuffering
    forkIO $ handle client (Paradise.PlayerData 3) False
    acceptAll sock

createServer :: PortNumber -> IO ()
createServer port = withSocketsDo $ do
    -- Create socket and accept everyone
    sock <- listenOn $ PortNumber port
    acceptAll sock
    sClose sock