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
import Control.Monad.Except
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as X
import qualified Data.Text.Lazy.Encoding as D
import FaeOutgoing.Coins
import FaeTX.Post
import FaeTX.Types
import qualified Network.WebSockets as WS
import Prelude
import Text.Pretty.Simple (pPrint)
import Types

msgHandler :: MVar ServerState -> Client -> Msg -> IO ()
msgHandler state client m@RequestCoinsMsg {}  = handleCoinRequest m client state
msgHandler state client@Client {..} CreateAuctionMsg = undefined
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

handleCoinRequest :: Msg -> Client -> MVar ServerState -> IO ()
handleCoinRequest (RequestCoinsMsg numCoins) client@Client{..} state = do
  newWallet <- runExceptT $ generateCoins key numCoins wallet
  either (sendErrMsg conn) (grantCoins state client numCoins) newWallet
  where
    key = Key "bidder"

grantCoins :: MVar ServerState -> Client -> Int -> Wallet -> IO ()
grantCoins state client@Client {..} numCoins newWallet = do
  ServerState {..} <- readMVar state
  updateServerState state ServerState {clients = updateClientWallet clients client newWallet, ..}
  sendMsgs (encodeMsg (CoinsGeneratedMsg numCoins)) [conn]
      

sendErrMsg :: WS.Connection -> PostTXError -> IO ()
sendErrMsg conn postTXError = sendMsgs msg [conn]
  where
    msg = encodeMsg $ ErrMsg postTXError
