{-# LANGUAGE OverloadedStrings #-}

module Server
( createServer
) where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Char8 as B
import           Network
import qualified Paradise.Client as Paradise
import qualified Paradise.API as API
import           System.IO
import           Text.Regex.PCRE

parse :: Handle -> Paradise.PlayerData -> B.ByteString
         -> IO Paradise.PlayerData
parse client pdata result
    | result =~ API.vtRegexp = do
                               (newdata, res) <- API.goto pdata result
                               B.hPutStrLn client res
                               return newdata
    | otherwise              = do
                               B.hPutStrLn client result
                               return pdata

command :: Handle -> Paradise.PlayerData -> B.ByteString
           -> IO Paradise.PlayerData
command client pdata line =
    API.act line pdata
    >>= parse client pdata
    >>= return

handle :: Handle -> Paradise.PlayerData -> Bool -> IO ()
handle client pdata True  = return ()
handle client pdata False = do
    -- Read input and execute it
    line  <- B.hGetLine client
    pdata <- command client pdata line
    -- Continue if there is more data
    handle client pdata =<< hIsEOF client

acceptAll :: Socket -> IO ()
acceptAll sock = do
    (client, _, _) <- accept sock
    hSetBuffering client NoBuffering
    forkIO $ handle client (Paradise.PlayerData "3" "13") False
    acceptAll sock

createServer :: PortNumber -> IO ()
createServer port = withSocketsDo $ do
    -- Create socket and accept everyone
    sock <- listenOn $ PortNumber port
    acceptAll sock
    sClose sock