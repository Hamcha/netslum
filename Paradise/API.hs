{-# LANGUAGE OverloadedStrings #-}

module Paradise.API
( act
, goto
, update
, vtRegexp
) where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy  as L
import qualified Data.List as List
import qualified Data.Map  as Map
import           Network.HTTP
import           Network.HTTP.Base
import           Paradise.Client
import           Paradise.Utils
import           Text.Regex.PCRE

baseParadise :: String
baseParadise = "http://paradise.xxiivv.com"

vtRegexp :: B.ByteString
vtRegexp = "::(.*)"

act :: B.ByteString -> PlayerData -> IO B.ByteString
act action pdata =
    simpleHTTP (getRequest url)
    >>= getResponseBody
    >>= (\x -> return $ B.pack x)
    where
    url = concat [baseParadise,
                  "/router.php?route=actions",
                  "&phrase=", urlEncode actionstr,
                  "&vessel=", B.unpack $ vessel pdata]
    actionstr = B.unpack action

get :: PlayerData -> IO B.ByteString
get pdata =
    simpleHTTP (getRequest url)
    >>= getResponseBody
    >>= (\x -> stripHTML $ B.pack x)
    >>= return
    where
    url = concat [baseParadise,
                  "/router.php?route=general",
                  "&vessel=", B.unpack $ vessel pdata]

update :: PlayerData -> IO (PlayerData, B.ByteString)
update pdata =
    get pdata >>= (\x -> return (pdata, write x) )

goto :: PlayerData -> B.ByteString -> IO (PlayerData, B.ByteString)
goto pdata action =
    get newdata >>= (\x -> return (newdata, write x) )
    where
    newdata = pdata{vessel = vesselId}
    vesselId = getResult $ action =~ vtRegexp
    getResult = last . head

write :: B.ByteString -> B.ByteString
write text =
    B.concat $ List.intersperse "\r\n\r\n" $ map B.pack values
    where
    values = map (\x -> Map.findWithDefault "" x vesseldata)
                 ["location","general","relative","tips"]
    vesseldata = mapif $ JSON.decode $ L.fromStrict text
                 :: (Map.Map String String)
    mapif (Just x) = x
    mapif Nothing  = Map.fromList [("","")]