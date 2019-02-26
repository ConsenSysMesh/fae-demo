{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TemplateHaskell #-}


module Model where

import Control.Exception
import Auction
import Prelude
import Data.Aeson as A
import Data.Bool
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.Char8 as C
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.JSString as GJS
import qualified Data.List as Li
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Text.Lazy as W
import Data.Text.Lazy.Encoding as X
import Data.Time.Calendar
import Data.Time.Clock
import GHC.Generics
import Miso
import Miso.String (MisoString)
import qualified Miso.String as S
import Text.Read
import SharedTypes
import Data.Proxy
import Servant.API
import Servant.Utils.Links
import Types
import Data.UUID.V4
import Control.Monad.IO.Class
import GHCJS.Marshal

import Debug.Trace

parseServerAction :: MisoString -> Action
parseServerAction m =
  case parsedServerAction of
    Just msg -> ServerAction msg
    Nothing -> AppAction Noop
  where
    parsedServerAction = decode (C.pack $ GJS.unpack m) :: Maybe Msg

getInitialModel currentURI =
  Model
    { 
      uri = currentURI
    , auctions = M.empty
    , received = S.ms ""
    , msg = Message $ S.ms ""
    , bidFieldValue = 0
    , genNumCoinsField = 1
    , maxBidCountField = 4
    , auctionStartValField = 1
    , showBidHistory = False
    , loggedInUsername = S.ms ""
    , loggedIn = False
    , selectedAuctionTXID = Nothing
    , accountBalance = 0
    , txLog = []
    }

updateModel :: Action -> Model -> Effect Action Model
updateModel (AppAction action) m = handleAppAction action m
updateModel (ServerAction action) m = handleServerAction action m

handleAppAction :: AppAction -> Model -> Effect Action Model
handleAppAction (HandleURI u) m = m { uri = u } <# do
  pure $ AppAction Noop

handleAppAction (ChangeURI u) m = m <# do
  pushURI u
  pure $ AppAction Noop

handleAppAction (MintCoinsAndBid aucTXID coinCount) model = 
  mintCoinsAndBid coinCount aucTXID model

handleAppAction (SendServerAction action@(BidRequest _ _)) Model{..} =
  Model{ bidFieldValue = 0, ..} <# do
    send action
    pure (AppAction Noop)

handleAppAction (SendServerAction action) model =
  model <# do
    send action
    pure (AppAction Noop)

handleAppAction (SelectAuction aucTXID) Model {..} =
  noEff Model {selectedAuctionTXID = Just aucTXID, ..}

handleAppAction (UpdateBidField maybeInt) Model {..} =
  Model {bidFieldValue = fromMaybe bidFieldValue maybeInt, ..} <# do
    pure (AppAction Noop)

handleAppAction (UpdateGenNumCoinsField maybeInt) Model {..} =
  Model {genNumCoinsField = fromMaybe 0 maybeInt, ..} <# do
    pure (AppAction Noop)
handleAppAction (SendMessage msg) model =
  model <# do send msg >> pure (AppAction Noop)

handleAppAction (HandleWebSocket (WebSocketMessage msg@(Message m))) model =
  model {received = m} <# do
    pure parsedServerAction
  where
    parsedServerAction = parseServerAction m

handleAppAction (UpdateMessage m) model = noEff model {msg = Message m}

handleAppAction Login Model {..} =
  Model {loggedIn = True, ..} <# do send loggedInUsername >> pure (AppAction (goLobby))

handleAppAction (UpdateUserNameField newUsername) Model {..} =
  noEff Model {loggedInUsername = newUsername, ..}

handleAppAction (UpdateNewMaxBidCountField newMaxBidCount) Model {..} =
  noEff Model { maxBidCountField = newMaxBidCount, ..}

handleAppAction (UpdateNewStartingValField newAuctionStartVal) Model {..} =
  noEff Model {auctionStartValField = newAuctionStartVal, ..}

handleAppAction ToggleShowBidHistory Model {..} =
  noEff Model {showBidHistory = not showBidHistory, ..}

-- gets the auction start params from the fields, constructs the auction request msg and then sends
handleAppAction SendCreateAuctionRequest m@Model {..} = 
  m <# do send action >> pure (AppAction Noop)
  where action = CreateAuctionRequest $ AuctionOpts auctionStartValField maxBidCountField

handleAppAction Noop model = noEff model

handleAppAction _ model = noEff model

handleServerAction :: Msg -> Model -> Effect Action Model
handleServerAction a@(AuctionCreated (Username username) aucTXID auction) Model {..} =
  Model {
    auctions = updatedAuctions,
    txLog = newTXLog,
    selectedAuctionTXID = if (S.ms username == loggedInUsername) then pure aucTXID else selectedAuctionTXID,
    ..} <# (if (S.ms username == loggedInUsername) then (pure $ AppAction goAuctionHome) else (pure $ AppAction Noop))
  where
    newTXLog = txLog ++ [getTXLogEntry a]
    updatedAuctions = createAuction aucTXID auction auctions

handleServerAction a@(BidSubmitted _  aucTXID bid@Bid{..}) m@Model {..} =
  noEff Model {
      auctions = updatedAuctions,
      accountBalance = accountBalance - bidValue, 
      txLog = newTXLog,
      ..}
  where
    updatedAuctions = bidOnAuction aucTXID bid auctions
    newTXLog = txLog ++ [getTXLogEntry a]

handleServerAction a@(CollectionSubmitted _ _ _ (CoinCollectionErr _ ) _ _) model = noEff model

handleServerAction a@(CollectionSubmitted _ _ _ collectionResult aucTXID newAuction) m@Model {..} =
  noEff Model {
      auctions = updatedAuctions,
      txLog = newTXLog,
      ..}
  where
    updatedAuctions = M.insert aucTXID newAuction auctions
    newTXLog = txLog ++ [getTXLogEntry a]

handleServerAction a@(CoinsGenerated txid (Username username) timestamp numCoins) Model {..} =
  noEff Model {accountBalance = newBalance, txLog = txLog ++ [getTXLogEntry a], ..} -- bidAmount == account balance for simplicity
  where newBalance = accountBalance + numCoins

handleServerAction _ model = noEff model

mintCoinsAndBid :: Int -> AucTXID -> Model -> Effect Action Model
mintCoinsAndBid targetBid aucTXID model@Model{..} = 
  traceShow (coinsNeededForBid) (updateModel mintCoinsMsg model >>= updateModel bidMsg) 
  where
    mintCoinsMsg = AppAction $ SendServerAction $ RequestCoins coinsNeededForBid
    bidMsg = AppAction $ SendServerAction $ BidRequest aucTXID targetBid
    noKeyErrMsg = "aucTXID key not found in auctions map"
    auc@Auction{..} = fromMaybe (error noKeyErrMsg) (M.lookup aucTXID auctions)
    currUserBidVal = Li.foldr (\Bid{..} acc ->
      if bidder == S.fromMisoString loggedInUsername then bidValue + acc else acc) 0 bids
    coinsNeededForBid = if Li.null bids then targetBid else targetBid - (getUserBidTotal auc (S.fromMisoString loggedInUsername))

getTXDescription :: Msg -> String
getTXDescription (AuctionCreated _ (AucTXID aucTXID) auction) = "Auction created"
getTXDescription (BidSubmitted _ (AucTXID aucTXID) Bid{..}) = Li.concat ["Raised bid to ", show bidValue, " ", bool "coin" "coins" (bidValue > 1)]
getTXDescription (CoinsGenerated _ (Username username) _ coinCount) = Li.concat ["Minted ", show coinCount, " ", bool "coin" "coins" (coinCount > 1)]
getTXDescription (CollectionSubmitted _ (Username clientName) currentTime collectionResult aucTXID newAuction) = case collectionResult of 
  LoserRefunded coinCount -> Li.concat ["Coins refunded", show coinCount, " ", bool "coin" "coins" (coinCount > 1)]
  BidsRetracted coinCount -> Li.concat ["Retracted bids and refunded", show coinCount, " ", bool "coin" "coins" (coinCount > 1)]
getTXDescription _ = " "

getTXLogEntry :: Msg -> TXLogEntry
getTXLogEntry msg@(BidSubmitted entryTXID (AucTXID aucTXID) Bid{..}) =
  TXLogEntry {..}
    where
      entryUsername = bidder
      entryTimestamp = bidTimestamp
      entryDescription = getTXDescription msg
getTXLogEntry msg@(AuctionCreated (Username entryUsername) (AucTXID entryTXID) Auction{..}) =
  TXLogEntry {..}
    where
      entryDescription = getTXDescription msg
      entryTimestamp = createdTimestamp
getTXLogEntry msg@(CoinsGenerated entryTXID (Username entryUsername) timestamp coinCount) =
  TXLogEntry {..}
    where
      entryDescription = getTXDescription msg
      entryTimestamp = timestamp
getTXLogEntry msg@(CollectionSubmitted entryTXID (Username entryUsername) timestamp collectionResult aucTXID newAuction) =
  TXLogEntry {..}
    where
      entryDescription = getTXDescription msg
      entryTimestamp = timestamp
