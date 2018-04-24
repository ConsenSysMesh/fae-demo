{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Clients where

import Control.Monad
import Data.Text (Text)
import qualified Network.WebSockets as WS
import Prelude

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

broadcast :: Text -> [WS.Connection] -> IO ()
broadcast message connections =
  forM_ connections $ \conn -> WS.sendTextData conn message
