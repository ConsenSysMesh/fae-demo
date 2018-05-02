{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Clients where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (finally)
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Foldable
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as X
import qualified Data.Text.Lazy.Encoding as D
import qualified Network.WebSockets as WS
import Prelude
import Text.Pretty.Simple (pPrint)

import Auction
import Types

addMsgHandler ::
     Client
  -> MVar ServerState
  -> (Msg -> Client -> ServerState -> IO a)
  -> IO b
addMsgHandler client@Client{..} state msgHandler =
  forever $ do
    msg <- WS.receiveData conn
    serverState <- readMVar state
    for_ (parseMsg msg) $ \parsedMsg -> msgHandler parsedMsg client serverState

clientExists :: Client -> [Client] -> Bool
clientExists client clients = client `elem` clients

addClient :: Client -> [Client] -> [Client]
addClient client clients = client : clients

removeClient :: Client -> [Client] -> [Client]
removeClient client = filter (/= client)

getClientConn :: Client -> WS.Connection
getClientConn Client{..} = conn

getClientWallet :: Client -> Wallet
getClientWallet Client{..} = wallet 

getClientWsConns :: [Client] -> [WS.Connection]
getClientWsConns = Prelude.map getClientConn

sendMsgs :: Text -> [WS.Connection] -> IO ()
sendMsgs msg connections = forM_ connections $ \conn -> WS.sendTextData conn msg

broadcast :: MVar ServerState -> Text -> IO ()
broadcast serverState msg =
  readMVar serverState >>=
  (\ServerState {..} -> do
     print
       ("outgoing to: [  " ++
        (show clients) ++ " ] ---------------> " ++ (show msg))
     sendMsgs msg (getClientWsConns clients))
     -- the output of PostTX should decide this

--broadcastValidAuctionActions ::
--     MVar ServerState -> Map String Auction -> Msg -> IO ()
--broadcast state auctions aucAction = broadcast state jsonMsg
--  where
--    jsonMsg = encodeMsg aucAction

encodeMsg :: Msg -> Text
encodeMsg a = T.pack $ show $ X.toStrict $ D.decodeUtf8 $ encode a

parseMsg :: Text -> Maybe Msg
parseMsg jsonTxt = decode $ C.pack $ T.unpack jsonTxt
