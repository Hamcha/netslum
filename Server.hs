{-# LANGUAGE OverloadedStrings #-}

module Server
( createServer
) where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Char8 as B
import           Network
import qualified Paradise.API as API
import qualified Paradise.Client as Paradise
import           Paradise.Utils
import           System.IO
import           Text.Regex.PCRE

parse :: Handle -> Paradise.PlayerData -> B.ByteString
         -> IO Paradise.PlayerData
parse client pdata result = do
    B.hPutStr client clearScreenCode
    B.hPutStr client homeRowCode
    let action p r | r =~ API.vtRegexp = API.goto p r
                   | otherwise         = do
                                         B.hPutStr client =<< stripHTML result
                                         B.hPutStr client "\r\n"
                                         API.update p
    (newdata, res) <- action pdata result
    B.hPutStr client res
    B.hPutStr client "\r\n\r\n"
    return newdata

command :: Handle -> Paradise.PlayerData -> B.ByteString
           -> IO Paradise.PlayerData
command client pdata "exit" = close client pdata
command client pdata "quit" = close client pdata
command client pdata line =
    API.act line pdata
    >>= parse client pdata
    >>= return

close :: Handle -> Paradise.PlayerData -> IO Paradise.PlayerData
close client pdata = do
    B.hPutStr client "\r\nGoodbye!\r\n"
    hClose client
    return pdata

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
    forkIO $ do
      let pdata = (Paradise.PlayerData "3")
      parse client pdata "::3"
      handle client pdata False
    acceptAll sock

createServer :: PortNumber -> IO ()
createServer port = withSocketsDo $ do
    -- Create socket and accept everyone
    sock <- listenOn $ PortNumber port
    acceptAll sock
    sClose sock