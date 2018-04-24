{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
  ( runServer
  ) where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Foldable
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as X
import qualified Data.Text.Lazy.Encoding as D
import qualified Network.WebSockets as WS
import Prelude
import Text.Pretty.Simple (pPrint)

import Auction
import Clients
import Types

initialServerState :: ServerState
initialServerState = ServerState {clients = [], auctions = IntMap.empty}

handleAuctionAction :: ServerState -> AuctionAction -> ServerState
handleAuctionAction ServerState {..} (CreateAuctionAction auction) =
  ServerState {auctions = createAuction auction auctions, ..}
handleAuctionAction ServerState {..} (BidAuctionAction auctionId bid) =
  ServerState {auctions = bidOnAuction auctionId bid auctions, ..}

parseAuctionAction :: Text -> Maybe AuctionAction
parseAuctionAction jsonTxt = decode $ C.pack $ T.unpack jsonTxt

encodeAuctionAction :: AuctionAction -> Text
encodeAuctionAction a = T.pack $ show $ X.toStrict $ D.decodeUtf8 $ encode a

isValidAuctionAction :: AuctionAction -> IntMap Auction -> Bool
isValidAuctionAction (BidAuctionAction aucId bid) auctions =
  case IntMap.lookup aucId auctions of
    (Just auction) -> validBid bid auction
    Nothing -> False
isValidAuctionAction (CreateAuctionAction Auction {..}) auctions =
  not $ IntMap.member auctionId auctions

-- update auction in serverState based on action
updateServerState :: MVar ServerState -> AuctionAction -> IO ()
updateServerState state action =
  modifyMVar_
    state
    (\serverState@ServerState {..} -> do
       print $ isValidAuctionAction action auctions
       return (handleAuctionAction serverState action))

sendMsg :: Text -> [Client] -> IO ()
sendMsg msg clients = do
  print
    ("outgoing to: [  " ++
     (show clients) ++ " ] ---------------> " ++ (show msg))
  forM_ clients $ \(Client (_, conn)) -> WS.sendTextData conn msg

broadcastAuctionAction :: MVar ServerState -> AuctionAction -> IO ()
broadcastAuctionAction state auctionAction =
  readMVar state >>= (\ServerState {..} -> sendMsg jsonMsg clients)
  where
    jsonMsg = encodeAuctionAction auctionAction

broadcastValidAuctionActions ::
     MVar ServerState -> IntMap Auction -> AuctionAction -> IO ()
broadcastValidAuctionActions state auctions act =
  when (isValidAuctionAction act auctions) $ broadcastAuctionAction state act

talk :: WS.Connection -> MVar ServerState -> IO ()
talk conn state =
  forever $ do
    msg <- WS.receiveData conn
    ServerState {..} <- readMVar state
    for_ (parseAuctionAction msg) $ \parsedAuctionAction -> do
      updateServerState state parsedAuctionAction
      broadcastValidAuctionActions state auctions parsedAuctionAction

--actual server. For this purpose, we use the simple server provided by
--`WS.runServer`.
runServer :: IO ()
runServer = do
  state <- newMVar initialServerState
  WS.runServer "127.0.0.1" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
--`WS.ServerApp` is nothing but a type synonym for
--`WS.PendingConnection -> IO ()`.
--ork a pinging thread in the background. This will ensure the connection
--stays alive on some browsers.
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  msg <- WS.receiveData conn
  ServerState {..} <- readMVar state
  case msg
--Check that the given username is not already taken:
        of
    _
      | clientExists client clients ->
        WS.sendTextData conn ("User already exists" :: Text)
      | otherwise ->
        flip finally disconnect $ do
          modifyMVar_ state $ \ServerState {..} -> do
            let newServerState =
                  ServerState {clients = (addClient client clients), ..}
            return newServerState
          talk conn state
      where clientName = T.filter (\c -> c `notElem` ['"', ' ']) msg
            client = Client (clientName, conn)
            -- disconnect is called when the connection is closed.
            disconnect
                -- Remove client and return new state
             = do
              s <-
                modifyMVar state $ \ServerState {..} ->
                  let s' =
                        ServerState {clients = removeClient client clients, ..}
                   in return (s', s')
              broadcast (clientName `mappend` " disconnected") [conn]
