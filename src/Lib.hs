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
  print (encodeMsg (RequestCoinsMsg 1))
  s@ServerState {..} <- readMVar state
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
                  ServerState{clients = addClient client clients, ..}
            return newServerState
          clientListener  client (msgHandler state) -- clean up state passing with readerT
          print (encodeMsg (RequestCoinsMsg 1))
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
              sendMsgs (clientName `mappend` " disconnected") [conn]
