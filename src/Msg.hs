{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

---------------------------------------------------------------------------
-- Post Transactions and Sync Server state and Broadcast msg to clients
---------------------------------------------------------------------------
module Msg
  ( msgHandler
  ) where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (finally)
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Foldable
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import Auction
import Clients
import Coins
import Control.Monad.Except
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as X
import qualified Data.Text.Lazy.Encoding as D
import FaeTX.Post
import FaeTX.Types
import qualified Network.WebSockets as WS
import Prelude
import Text.Pretty.Simple (pPrint)
import Types
import Utils

msgHandler :: MVar ServerState -> Text -> Msg -> IO ()
msgHandler state clientName m@RequestCoinsMsg {}  = handleCoinRequest m clientName state
msgHandler state clientName CreateAuctionMsg = undefined
 --    where  key = "bidder1"
 --           postTXResult = bid key aucId amount

--msgHandler (BidMsg aucId amount) Client{..} ServerState {..} = undefined
--msgHandler (RequestCoinsMsg numCoins) client state ServerState {..} = getCoinsRequestHandler numCoins client state
--     where  key = "bidder1"
--            clientWallet = getClientWallet
-- update auction in serverState based on action
updateServerState :: MVar ServerState -> ServerState -> IO ()
updateServerState state newServerState =
  modifyMVar_
    state
    (\serverState@ServerState {..} -> do
       pPrint $ newServerState
       return newServerState)

handleCoinRequest :: Msg -> Text -> MVar ServerState -> IO ()
handleCoinRequest (RequestCoinsMsg numCoins) clientName state = do
  ServerState{..} <- readMVar state
  let Client{..} = fromJust $ getClient clients clientName
  pPrint (show wallet ++ "clients wallet before generating coins")
  newWallet <- runExceptT $ generateCoins key numCoins wallet
  either  (sendErrMsg conn) (grantCoins state clientName numCoins) newWallet
  where
    key = Key "bidder"
    sendErrMsg conn postTXErr = sendMsg (encodeMsg $ ErrMsg postTXErr) conn

grantCoins :: MVar ServerState -> Text -> Int -> Wallet -> IO ()
grantCoins state clientName numCoins newWallet = do
  ServerState {..} <- readMVar state
  let client@Client{..} = fromJust $ getClient clients clientName
  updateServerState state ServerState {clients = updateClientWallet clients client newWallet, ..}
  sendMsg (encodeMsg (CoinsGeneratedMsg numCoins)) conn

