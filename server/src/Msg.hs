{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Msg
  ( msgHandler
  ) where

import Data.Time.Calendar
import Data.Time.Format
import Data.Time.Clock
import AuctionManager
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
import Data.Map.Lazy (Map)
import  qualified Data.Map.Lazy as M
import Types
import Utils
import qualified Network.WebSockets as WS
import Control.Monad.Reader
import PostTX
import SharedTypes
import Text.Pretty.Simple (pPrint)

msgHandler :: Msg -> ReaderT (MVar ServerState, String) IO ()
msgHandler (RequestCoins numCoins) =  handleCoinRequest numCoins
msgHandler (BidRequest aucTXID amount) = handleBidRequest aucTXID amount
msgHandler (CreateAuctionRequest auctionOpts) = handleCreateAuctionRequest auctionOpts

updateServerState :: MVar ServerState -> ServerState -> IO ()
updateServerState state newServerState =
  modifyMVar_
    state
    (\serverState@ServerState {..} -> do
       pPrint serverState
       return newServerState)

handleFaeOutput :: Either PostTXError PostTXResponse -> ReaderT (MVar ServerState, String) IO ()
handleFaeOutput output = do 
    (state, clientName) <- ask
    ServerState {..} <- liftIO $ readMVar state
    let Client{..} = fromJust $ getClient clients $ T.pack clientName --ugh fix fromJust
    either (\err -> liftIO $ sendMsg conn $ ErrMsg $ PostTXErr err) updateAuction output

handleBidRequest :: AucTXID -> Int -> ReaderT (MVar ServerState, String) IO ()
handleBidRequest aucTXID amount= do 
  (state, clientName) <- ask
  ServerState {..} <- liftIO $ readMVar state
  let client@Client{..} = fromJust $ getClient clients $ T.pack clientName --ugh fix fromJust --also just change the client name to be Text
  let clientWallet = getWallet wallet
  let coinTXID = head $ M.keys clientWallet
  if not $ M.null clientWallet then do
       faeOut <- liftIO $ postBidTX key aucTXID coinTXID
       ServerState {..} <- liftIO $ readMVar state
       handleFaeOutput faeOut
  else liftIO $ sendMsg conn $ ErrMsg NoCoins
  where
    key = Key "bidder1"
    getWallet (Wallet wallet) = wallet

handleCreateAuctionRequest :: AuctionOpts -> ReaderT (MVar ServerState, String) IO ()
handleCreateAuctionRequest AuctionOpts{..} = do
  liftIO $ updateStartingBid startingVal
  liftIO $ updateMaxBidCount maxBidCount
  faeOut <- liftIO $ postCreateAuctionTX key
  handleFaeOutput faeOut
  where
    key = Key "bidder1"

updateAuction :: PostTXResponse -> ReaderT (MVar ServerState, String) IO ()
updateAuction (BidTX txid aucTXID coinTXID hasWon) = do
  (state, clientName) <- ask
  ServerState {..} <- liftIO $ readMVar state
  currentTime <- liftIO getCurrentTime
  let Client{..} = fromJust $ getClient clients $ T.pack clientName --ugh fix fromJust
  let unwrappedWallet = getWallet wallet
  let bidAmount = fromMaybe 0 $ M.lookup coinTXID unwrappedWallet
  newBid <- liftIO $ getNewBid clientName bidAmount
  let updatedAuctions = updateAuctionWithBid aucTXID newBid auctions
  let newWallet = Wallet $ M.delete coinTXID $ unwrappedWallet  -- remove spent coin cache
  liftIO $ updateServerState state ServerState {clients = updateClientWallet clients name newWallet, auctions = updatedAuctions}
  liftIO $ broadcast state $ outgoingMsg newBid
  where
    getWallet (Wallet wallet) = wallet
    getNewBid clientName bidAmount = do
      currentTime <- getCurrentTime
      return Bid {bidder = clientName, bidValue = bidAmount, bidTimestamp = currentTime, isWinningBid = hasWon}
    outgoingMsg = BidSubmitted aucTXID 
updateAuction (AuctionCreatedTX (txid)) = do
  (state, clientName) <- ask
  ServerState {..} <- liftIO $ readMVar state
  newAuction <- liftIO $ getNewAuction clientName
  let updatedAuctions = createAuction auctionId newAuction auctions
  liftIO $ updateServerState state ServerState {auctions = updatedAuctions, ..}
  liftIO $ broadcast state $ outgoingMsg $ newAuction
  where
    auctionId = AucTXID $ show txid
    getNewAuction clientName = do
      currentTime <- getCurrentTime
      return Auction
        {createdBy = clientName, bids = [], createdTimestamp = currentTime }
    outgoingMsg = AuctionCreated auctionId

handleCoinRequest :: Int ->  ReaderT (MVar ServerState, String) IO ()
handleCoinRequest numCoins  = do
  (state, clientName) <- ask
  ServerState {..} <- liftIO $ readMVar state
  let Client{..} = fromJust $ getClient clients (T.pack clientName) --ugh fix fromJust
  newWallet <- liftIO $ runExceptT $ generateCoins key numCoins wallet
  either (liftIO . sendMsg conn . ErrMsg . PostTXErr) (grantCoins numCoins) newWallet
  where key = Key "bidder1"

grantCoins :: Int -> Wallet -> ReaderT (MVar ServerState, String) IO ()
grantCoins numCoins newWallet = do
  (state, clientName) <- ask
  ServerState {..} <- liftIO $ readMVar state
  let client@Client{..} = fromJust $ getClient clients (T.pack clientName) --ugh fix fromJust
  liftIO $ updateServerState state ServerState {clients = updateClientWallet clients  name newWallet, ..}
  liftIO $ sendMsg conn $ CoinsGenerated numCoins

  