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
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as X
import qualified Data.Text.Lazy.Encoding as D
import qualified Network.WebSockets as WS
import Prelude
import Text.Pretty.Simple (pPrint)

import Auction
import Types

--Create a new, initial state:
initialServerState :: ServerState
initialServerState = ServerState {clients = [], auctions = IntMap.empty}

--Check if a user already exists (based on username):
clientExists :: Client -> ServerState -> Bool
clientExists client ServerState {..} = client `elem` clients

--Add a client (this does not check if the client already exists, you should do
--this yourself using `clientExists`):
addClient :: Client -> ServerState -> ServerState
addClient client ServerState {..} = ServerState {clients = client : clients, ..}

removeClient :: Client -> ServerState -> ServerState
removeClient client ServerState {..} =
  ServerState {clients = filteredClients, ..}
  where
    filteredClients = filter (/= client) clients

handleAuctionAction :: ServerState -> AuctionAction -> ServerState
handleAuctionAction ServerState {..} (CreateAuctionAction auction) =
  ServerState {auctions = createAuction auction auctions, ..}
handleAuctionAction ServerState {..} (BidAuctionAction auctionId bid) =
  ServerState {auctions = bidOnAuction auctionId bid auctions, ..}

--Send a message to all clients, and log it on stdout:
broadcast :: Text -> ServerState -> IO ()
broadcast message ServerState {..} = do
  T.putStrLn message
  forM_ clients $ \(Client (_, conn)) -> WS.sendTextData conn message

parseAuctionAction :: Text -> Maybe AuctionAction
parseAuctionAction jsonTxt = decode $ C.pack $ T.unpack jsonTxt

encodeAuctionAction :: AuctionAction -> Text
encodeAuctionAction a = T.pack $ show $ X.toStrict $ D.decodeUtf8 $ encode a

-- update auction in serverState based on action
updateServerState :: MVar ServerState -> AuctionAction -> IO ()
updateServerState state action =
  modifyMVar_
    state
    (\serverState -> return (handleAuctionAction serverState action))

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

isValidAuctionAction :: AuctionAction -> IntMap Auction -> Bool
isValidAuctionAction (BidAuctionAction aucId bid) auctions =
  case IntMap.lookup aucId auctions of
    (Just auction) -> validBid bid auction
    Nothing -> False
isValidAuctionAction (CreateAuctionAction Auction {..}) auctions =
  IntMap.member auctionId auctions

broadcastValidAuctionActions ::
     MVar ServerState -> IntMap Auction -> AuctionAction -> IO ()
broadcastValidAuctionActions state auctions act =
  when (isValidAuctionAction act auctions) $ broadcastAuctionAction state act

talk conn state (Client (name, _)) =
  forever $ do
    msg <- WS.receiveData conn
    serverState@ServerState {..} <- readMVar state
    pPrint $ "our unparsed ws message from" ++ T.unpack name
    pPrint $ T.unpack msg
    pPrint $ "our parsed ws message from: " ++ T.unpack name
    let parsedAuctionAction = parseAuctionAction msg
    pPrint $ parseAuctionAction msg
    pPrint serverState
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
--When a client is succesfully connected, we read the first message. This should
--be in the format of "Hi! I am Jasper", where Jasper is the requested username.
  msg <- WS.receiveData conn
  clients <- readMVar state
  case msg
--Check that the given username is not already taken:
        of
    _
      | clientExists client clients ->
        WS.sendTextData conn ("User already exists" :: Text)
      | otherwise ->
        flip finally disconnect $ do
          modifyMVar_ state $ \s -> do
            let s' = addClient client s
            T.putStrLn clientName
            putStrLn $ C.unpack $ encode sampleAction
            putStrLn $ C.unpack $ encode sampleAction2
            --broadcast stringifiedJsonAction s'
            return s'
          talk conn state client
      where clientName = T.filter (\c -> c `notElem` ['"', ' ']) msg
            client = Client (clientName, conn)
            sampleAction =
              CreateAuctionAction
                Auction
                  { auctionId = 1
                  , value = 1
                  , bids = []
                  , createdBy = "Argo"
                  , createdTimestamp = "0200"
                  , maxNumBids = 3
                  }
            sampleAction2 =
              BidAuctionAction
                1
                Bid {bidValue = 3, bidder = "Xena", bidTimestamp = "22:10"}
            jsonAction = X.toStrict $ D.decodeUtf8 $ encode sampleAction
            stringifiedJsonAction = T.pack $ show jsonAction
            -- disconnect is called when the connection is closed.
            disconnect
                -- Remove client and return new state
             = do
              s <-
                modifyMVar state $ \s ->
                  let s' = removeClient client s
                   in return (s', s')
              broadcast (clientName `mappend` " disconnected") s
