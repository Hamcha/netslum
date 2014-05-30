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
parse client pdata result
    | result =~ API.vtRegexp = do
                               (newdata, res) <- API.goto pdata result
                               B.hPutStrLn client clearScreenCode
                               B.hPutStrLn client homeRowCode
                               B.hPutStrLn client res
                               B.hPutStrLn client "\r\n"
                               return newdata
    | otherwise              = do
                               (newdata, res) <- API.update pdata
                               B.hPutStrLn client clearScreenCode
                               B.hPutStrLn client homeRowCode
                               B.hPutStrLn client =<< stripHTML result
                               B.hPutStrLn client "\r\n"
                               B.hPutStrLn client res
                               B.hPutStrLn client "\r\n"
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
    B.hPutStrLn client "\r\nGoodbye!"
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