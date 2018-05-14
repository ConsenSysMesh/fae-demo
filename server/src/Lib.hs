{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
  ( runServer
  ) where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (finally)
import Data.Foldable
import qualified Data.Map.Lazy as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import Prelude

import Clients
import Msg
import Types

initialServerState :: ServerState
initialServerState = ServerState {clients = [], auctions = Map.empty}

runServer :: IO ()
runServer = do
  state <- newMVar initialServerState
  WS.runServer "127.0.0.1" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  msg <- WS.receiveData conn
  ServerState {..} <- readMVar state
  case msg
        of
    _
      | clientExists client clients ->
        WS.sendTextData conn ("User already exists" :: Text)
      | otherwise ->
        flip finally disconnect $ do
          modifyMVar_ state $ \ServerState {..} -> do
            print clientName
            return ServerState {clients = addClient client clients, ..}
          clientListener state clientName conn msgHandler 
      where clientName = T.filter (\c -> c `notElem` ['"', ' ']) msg
            client =
              Client {name = clientName, conn = conn, wallet = Wallet Map.empty}
            -- disconnect is called when the connection is closed.
            disconnect
                -- Remove client and return new state
             = do
              s <-
                modifyMVar state $ \ServerState {..} ->
                  let s' =
                        ServerState {clients = removeClient client clients, ..}
                   in return (s', s')
              return ()
