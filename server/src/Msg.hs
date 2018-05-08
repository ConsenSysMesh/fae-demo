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
import Data.List
import qualified Data.Text as T
import Prelude
import Text.Pretty.Simple (pPrint)
import Data.Map.Lazy (Map)
import  qualified Data.Map.Lazy as M
import Types
import Utils
import qualified Network.WebSockets as WS

import PostTX

msgHandler state client (RequestCoins numCoins) =
  handleCoinRequest state client numCoins
msgHandler state client (BidRequest aucTXID amount) =
  handleBidRequest state client aucTXID
msgHandler state client CreateAuctionRequest =
  handleCreateAuctionRequest state client


updateServerState :: MVar ServerState -> ServerState -> IO ()
updateServerState state newServerState =
  modifyMVar_
    state
    (\serverState@ServerState {..} -> do
       pPrint $ newServerState
       return newServerState)
-- TODO use reader to pass client and state around  
handleFaeOutput :: MVar ServerState -> Client -> Either PostTXError PostTXResponse -> IO ()
handleFaeOutput state client@Client{..} =
    either (sendMsg conn . ErrMsg . PostTXErr) (updateAuction state client)

-- use cont monad T to spend coin if bid is submitted successfully
handleBidRequest state client@Client{..} aucTXID =
  if not $ M.null clientWallet then do
      let coinTXID = head $ M.keys clientWallet
      faeOut <- postBidTX key aucTXID coinTXID
      ServerState {..} <- readMVar state
      handleFaeOutput state client faeOut
  else  sendMsg conn $ ErrMsg NoCoins
  where
    key = Key "bidder1"
    getWallet (Wallet wallet) = wallet
    clientWallet = getWallet wallet
-- coinTXID should be retrieved based on amount of coins to bid at the
-- moment bid amounts always equal account balance for simplicity to avoid implementing a wallet

handleCreateAuctionRequest state client@Client{..} = do
  faeOut <- postCreateAuctionTX key
  ServerState {..} <- readMVar state
  handleFaeOutput state client faeOut
  where
    key = Key "bidder1"

updateAuction :: MVar ServerState -> Client -> PostTXResponse -> IO ()
updateAuction state client@Client{..} (BidTX txid aucTXID coinTXID hasWon) = do
  ServerState {..} <- readMVar state
  let updatedAuctions = updateAuctionWithBid aucTXID newBid auctions
  updateServerState state ServerState {auctions = updatedAuctions, ..}
  broadcast state outgoingMsg
  let newWallet = Wallet $ M.delete coinTXID clientWallet -- remove spent coin cache
  updateServerState state ServerState {clients = updateClientWallet clients client newWallet, ..}
  where
    getWallet (Wallet wallet) = wallet
    clientWallet = getWallet wallet
    bidAmount = fromMaybe 0 $ M.lookup coinTXID clientWallet
    newBid =
      Bid {bidder = T.unpack name, bidValue = bidAmount, bidTimestamp = getTimestamp}
    outgoingMsg = BidSubmitted aucTXID newBid 
updateAuction state client@Client{..} (AuctionCreatedTX (TXID txid)) = do
  ServerState {..} <- readMVar state
  let updatedAuctions = createAuction auctionId newAuction auctions
  updateServerState state ServerState {auctions = updatedAuctions, ..}
  broadcast state outgoingMsg
  where
    auctionId = AucTXID txid
    newAuction =
      Auction
        {createdBy = T.unpack name, bids = [], createdTimestamp = getTimestamp}
    outgoingMsg = AuctionCreated auctionId newAuction

handleCoinRequest ::MVar ServerState -> Client -> Int -> IO ()
handleCoinRequest state client@Client{..} numCoins  = do
  ServerState {..} <- readMVar state
  pPrint (show wallet ++ "clients wallet before generating coins")
  newWallet <- runExceptT $ generateCoins key numCoins wallet
  either
    (sendMsg conn . ErrMsg . PostTXErr)
    (grantCoins state client numCoins)
    newWallet
  where
    key = Key "bidder"

grantCoins :: MVar ServerState -> Client -> Int -> Wallet -> IO ()
grantCoins state client@Client{..} numCoins newWallet = do
  ServerState {..} <- readMVar state
  updateServerState
    state
    ServerState {clients = updateClientWallet clients client newWallet, ..}
  sendMsg conn $ CoinsGenerated numCoins

  