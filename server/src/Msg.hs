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
import Data.Either
import Data.Maybe
import Control.Monad.Trans.State.Lazy
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
import Debug.Trace
import PostTX.Types
import FaeFrontend
import Text.Pretty.Simple (pPrint)

msgHandler :: Msg -> ReaderT (MVar ServerState, String) IO ()
msgHandler (RequestCoins numCoins) = handleCoinRequest numCoins
msgHandler a@(BidRequest aucTXID amountToBidUpTo) = traceShow a (handleBidRequest aucTXID amountToBidUpTo)
msgHandler (CreateAuctionRequest auctionOpts) = handleCreateAuctionRequest auctionOpts
msgHandler a@(CollectRequest aucTXID) = traceShow a (handleCollectRequest aucTXID) 

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
    let Client{..} = fromMaybe (error "Couldn't find client with given name in map") (getClient clients $ T.pack clientName) 
    either (\err -> liftIO $ sendMsg conn $ ErrMsg $ PostTXErr err) handleFaeResponse output

handleCollectRequest :: AucTXID -> ReaderT (MVar ServerState, String) IO ()
handleCollectRequest aucTXID = do
  liftIO $ putStrLn "Requested to collect - "
  (state, clientName) <- ask
  let key = Key clientName
  ServerState {..} <- liftIO $ readMVar state
  liftIO $ print "---------POSTING COLLECT TX-----------"
  faeOut <- liftIO $ postCollectTX key aucTXID
  ServerState {..} <- liftIO $ readMVar state
  handleFaeOutput faeOut

handleBidRequest :: AucTXID -> Int -> ReaderT (MVar ServerState, String) IO ()
handleBidRequest aucTXID amountToBidUpTo = do
  liftIO $ putStrLn $ concat ["Requested to bid - up to ", show amountToBidUpTo]
  (state, clientName) <- ask
  let key = Key clientName
  ServerState {..} <- liftIO $ readMVar state
  let client@Client{..} = fromJust $ getClient clients $ T.pack clientName --ugh fix fromJust --also just change the client name to be Text
  let clientWallet = getWallet wallet
  let coinTXID = head $ M.keys clientWallet
  liftIO $ print "------ is client wallet not null"
  liftIO $ print $ not $ M.null clientWallet
  if not $ M.null clientWallet then do
       faeOut <- liftIO $ postBidTX key aucTXID coinTXID
       liftIO $ print faeOut
       ServerState {..} <- liftIO $ readMVar state
       handleFaeOutput faeOut
  else liftIO $ sendMsg conn $ ErrMsg NoCoins
  where getWallet (Wallet wallet) = wallet

handleCreateAuctionRequest :: AuctionOpts -> ReaderT (MVar ServerState, String) IO ()
handleCreateAuctionRequest AuctionOpts{..} = do
  (state, clientName) <- ask
  liftIO $ updateStartingBid startingVal
  liftIO $ updateMaxBidCount maxBidCount
  let key = Key clientName
  faeOut <- liftIO $ postCreateAuctionTX key startingVal maxBidCount
  handleFaeOutput faeOut

handleFaeResponse :: PostTXResponse -> ReaderT (MVar ServerState, String) IO ()
handleFaeResponse a@(BidTX txid aucTXID coinTXID (TXResult txResult))
  | txResult /= "\"You won!\"" && txResult  /= "\"Bid accepted\"" = traceShow ("EERRROR")  (error ("Bid was not accepted by Fae, error: " ++ txResult)) 
  | otherwise = do
    (state, clientName) <- ask
    liftIO $ print a
    ServerState {..} <- liftIO $ readMVar state
    currentTime <- liftIO getCurrentTime
    let 
      Client{..} = fromMaybe (error "couldn't find the client with given name") (getClient clients $ T.pack clientName)
      unwrappedWallet = getWallet wallet
      auction@Auction{..} = fromMaybe (error "no such aucTXID") (M.lookup aucTXID auctions)
      hasRetractedPrevBids = any ((==) $ Username clientName) bidsRetracted
      userTotalBidSoFar = if hasRetractedPrevBids then 0 else getUserBidTotal auction clientName
      userRaisedTheirBidBy = fromMaybe (error "no such CoinTXID") $ M.lookup coinTXID unwrappedWallet
      auctionsNewCurrBidVal = userRaisedTheirBidBy + userTotalBidSoFar
    bid <- liftIO $ getBid clientName auctionsNewCurrBidVal
    liftIO $ putStrLn "USER Total bid so far - "
    liftIO $ print userTotalBidSoFar
    liftIO $ putStrLn "USER RAISED THEIR OWN BID BY IS - "
    liftIO $ print userRaisedTheirBidBy
    liftIO $ putStrLn "NEW AUCTION BID AMOUNT REPORTED IS - "
    liftIO $ print auctionsNewCurrBidVal
    let 
      updatedAuctions = updateAuctionWithBid aucTXID bid auctions
      newWallet = Wallet $ M.delete coinTXID $ unwrappedWallet  -- remove spent coin cache
    liftIO $ updateServerState state ServerState {clients = updateClientWallet clients name newWallet, auctions = updatedAuctions}
    liftIO $ broadcast state $ outgoingMsg bid
    where
      getWallet (Wallet wallet) = wallet
      getBid clientName bidAmount = do
        currentTime <- getCurrentTime
        return Bid {bidder = clientName, bidValue = bidAmount, bidTimestamp = currentTime, isWinningBid = txResult == "You won!"}
      outgoingMsg = BidSubmitted (show txid) aucTXID 

handleFaeResponse (AuctionCreatedTX txid (AucStartingValue startingValue) (MaxBidCount aucMaxBidCount)) = do
  (state, clientName) <- ask
  let outgoingMsg = AuctionCreated (Username clientName) auctionId
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
        {createdBy = clientName, bids = [], bidsRetracted = [], bidsRefunded = [], createdTimestamp = currentTime, .. }

handleFaeResponse (CollectTX txid aucTXID) = do
  (state, clientName) <- ask
  let username = (Username clientName)
  currentTime <- liftIO $ getCurrentTime
  ServerState {..} <- liftIO $ readMVar state
  let auc = fromMaybe (error "no such aucTXID") (M.lookup aucTXID auctions)
  let (newAuction, collectionResult) = collect username auc
  let updatedAuctions = M.insert aucTXID newAuction auctions
  liftIO $ updateServerState state ServerState {auctions = updatedAuctions, ..}
  liftIO $ broadcast state $ CollectionSubmitted (show txid) (Username clientName) currentTime collectionResult aucTXID newAuction

handleCoinRequest :: Int -> ReaderT (MVar ServerState, String) IO ()
handleCoinRequest numCoins  = do
  liftIO $ putStrLn $ concat ["Requested generation of ", show numCoins]
  (state, clientName) <- ask
  let key = Key clientName
  ServerState {..} <- liftIO $ readMVar state
  let Client{..} = fromMaybe (error "client doesn\'t exist in Map") (getClient clients (T.pack clientName))
  postTXResult <- liftIO $ runStateT (runExceptT $ generateCoins key numCoins wallet) wallet
  either (liftIO . sendMsg conn . ErrMsg . PostTXErr) ((flip (grantCoins numCoins)) (snd postTXResult)) (fst postTXResult)

grantCoins :: Int -> TransactionID -> Wallet -> ReaderT (MVar ServerState, String) IO ()
grantCoins numCoins txid newWallet = do
  (state, clientName) <- ask
  currentTime <- liftIO $ getCurrentTime
  ServerState {..} <- liftIO $ readMVar state
  let client@Client{..} =  fromMaybe (error "client doesn\'t exist in Map") (getClient clients (T.pack clientName))
  liftIO $ updateServerState state ServerState {clients = updateClientWallet clients name newWallet, ..}
  liftIO $ sendMsg conn $ CoinsGenerated (show txid) (Username clientName) currentTime numCoins
