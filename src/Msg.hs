{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Msg
  ( msgHandler
  ) where

import Auction
import Clients
import Coins
import Control.Concurrent (MVar, modifyMVar, modifyMVar_, readMVar)
import Control.Monad
import Control.Monad.Except
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import FaeTX.Types hiding (Bid)
import Prelude
import Text.Pretty.Simple (pPrint)
import Types
import Utils

msgHandler :: MVar ServerState -> Text -> Msg -> IO ()
msgHandler state clientName (RequestCoins numCoins) =
  handleCoinRequest numCoins clientName state
msgHandler state clientName m@CreateAuctionRequest =
  handleAuctionMsg clientName state m
msgHandler state clientName m@BidRequest {} =
  handleAuctionMsg clientName state m

updateServerState :: MVar ServerState -> ServerState -> IO ()
updateServerState state newServerState =
  modifyMVar_
    state
    (\serverState@ServerState {..} -> do
       pPrint $ newServerState
       return newServerState)

handleAuctionMsg :: Text -> MVar ServerState -> Msg -> IO ()
handleAuctionMsg clientName state msg = do
  postTXResponse <- postCreateAuctionTX key
  ServerState {..} <- readMVar state
  let Client {..} = fromJust $ getClient clients clientName
  either
    (sendMsg conn . ErrMsg)
    (updateAuction state (T.unpack clientName) msg)
    postTXResponse
  where
    key = Key "bidder1"

updateAuction :: MVar ServerState -> String -> Msg -> PostTXResponse -> IO ()
updateAuction state clientName (BidRequest aucTXID amount) _ = do
  ServerState {..} <- readMVar state
  let updatedAuctions = updateAuctionWithBid aucTXID newBid auctions
  updateServerState state ServerState {auctions = updatedAuctions, ..}
  broadcast state outgoingMsg
  where
    newBid =
      Bid {bidder = clientName, bidValue = amount, bidTimestamp = getTimestamp}
    outgoingMsg = BidSubmitted aucTXID newBid
updateAuction state clientName CreateAuctionRequest (CreateAuction (TXID txid)) = do
  ServerState {..} <- readMVar state
  let updatedAuctions = createAuction auctionId newAuction auctions
  updateServerState state ServerState {auctions = updatedAuctions, ..}
  broadcast state outgoingMsg
  where
    auctionId = AucTXID txid
    newAuction =
      Auction
        {createdBy = clientName, bids = [], createdTimestamp = getTimestamp}
    outgoingMsg = AuctionCreated auctionId newAuction

handleCoinRequest :: Int -> Text -> MVar ServerState -> IO ()
handleCoinRequest numCoins clientName state = do
  ServerState {..} <- readMVar state
  let Client {..} = fromJust $ getClient clients clientName
  pPrint (show wallet ++ "clients wallet before generating coins")
  newWallet <- runExceptT $ generateCoins key numCoins wallet
  either
    (sendMsg conn . ErrMsg)
    (grantCoins state clientName numCoins)
    newWallet
  where
    key = Key "bidder"

grantCoins :: MVar ServerState -> Text -> Int -> Wallet -> IO ()
grantCoins state clientName numCoins newWallet = do
  ServerState {..} <- readMVar state
  let client@Client {..} = fromJust $ getClient clients clientName
  updateServerState
    state
    ServerState {clients = updateClientWallet clients client newWallet, ..}
  sendMsg conn $ CoinsGenerated numCoins
