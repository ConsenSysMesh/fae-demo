{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Clients where

import ClientMsg.Types
import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Monad
import Data.Text (Text)
import qualified Network.WebSockets as WS
import Prelude

import Control.Exception (finally)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Foldable
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as X
import qualified Data.Text.Lazy.Encoding as D
import Text.Pretty.Simple (pPrint)

import Auction
import ClientMsg.Outgoing
import Types

clientExists :: Client -> [Client] -> Bool
clientExists client clients = client `elem` clients

addClient :: Client -> [Client] -> [Client]
addClient client clients = client : clients

removeClient :: Client -> [Client] -> [Client]
removeClient client clients = filter (/= client) clients

getClientConn :: Client -> WS.Connection
getClientConn (Client (_, conn)) = conn

getClientWsConns :: [Client] -> [WS.Connection]
getClientWsConns clients = Prelude.map getClientConn clients

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

isValidAuctionAction :: ClientMsg.Types.AuctionAction -> IntMap Auction -> Bool
isValidAuctionAction (BidAuctionAction aucId bid) auctions =
  case IntMap.lookup aucId auctions of
    (Just auction) -> validBid bid auction
    Nothing -> False
isValidAuctionAction (CreateAuctionAction Auction {..}) auctions =
  not $ IntMap.member auctionId auctions

broadcastValidAuctionActions ::
     MVar ServerState
  -> IntMap Auction
  -> ClientMsg.Types.AuctionAction
  -> IO ()
broadcastValidAuctionActions state auctions aucAction =
  when (isValidAuctionAction aucAction auctions) $ broadcast state jsonMsg
  where
    jsonMsg = encodeAuctionAction aucAction
