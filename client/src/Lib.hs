{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lib where

import Auction
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
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Text.Lazy as W
import Data.Text.Lazy.Encoding as X
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import GHC.Generics
import Miso
import Miso.String (MisoString)
import qualified Miso.String as S
import Text.Read

import Types
import Views

parseAuctionAction :: MisoString -> Action
parseAuctionAction m =
  case parsedAuctionAction of
    Just auctionAction -> AuctionAction auctionAction
    Nothing -> AppAction Noop
  where
    parsedAuctionAction = decode (C.pack $ GJS.unpack m) :: Maybe AuctionAction

getInitialModel :: Model
getInitialModel =
  Model
    { auctions = IntMap.empty
    , received = S.ms ""
    , msg = Message $ S.ms ""
    , bidFieldValue = 0
    , username = S.ms ""
    , loggedIn = False
    , selectedAuctionId = 0
    }

runApp :: IO ()
runApp = startApp App {initialAction = AppAction Noop, ..}
  where
    model = getInitialModel
    events = defaultEvents
    subs = [websocketSub uri protocols (AppAction . HandleWebSocket)]
    update = updateModel
    view = appView
    uri = URL "ws://localhost:9160"
    protocols = Protocols []
    mountPoint = Nothing

updateModel :: Action -> Model -> Effect Action Model
updateModel (AppAction (SendAuctionAction (CreateAuctionAction Auction {..}))) model =
  model <# do
    time <- getCurrentTime
    send (CreateAuctionAction (Auction {createdTimestamp = time, ..}))
    pure (AppAction Noop)
updateModel (AppAction (SendAuctionAction (BidAuctionAction aucId Bid {..}))) model =
  model <# do
    time <- getCurrentTime
    send (BidAuctionAction aucId (Bid {bidTimestamp = time, ..}))
    pure (AppAction Noop)
updateModel (AppAction (SelectAuction auctionId)) Model {..} =
  noEff Model {selectedAuctionId = auctionId, ..}
updateModel (AppAction (UpdateBidField maybeInt)) Model {..} =
  Model {bidFieldValue = fromMaybe bidFieldValue maybeInt, ..} <# do
    print maybeInt
    pure (AppAction Noop)
updateModel (AppAction (SendMessage msg)) model =
  model <# do print msg >> send msg >> pure (AppAction Noop)
updateModel (AppAction (HandleWebSocket (WebSocketMessage msg@(Message m)))) model =
  model {received = m} <# do
    print msg
    Prelude.putStrLn $ GJS.unpack m
    --print parsedAuctionAction
    --send parsedAuctionAction
    print model
    pure parsedAuctionAction
  where
    parsedAuctionAction = parseAuctionAction m
updateModel (AppAction (UpdateMessage m)) model = noEff model {msg = Message m}
updateModel (AuctionAction a@(CreateAuctionAction auction)) Model {..} =
  noEff Model {auctions = updatedAuctions, ..}
  where
    updatedAuctions = createAuction auction auctions
updateModel (AuctionAction a@(BidAuctionAction auctionId bid)) Model {..} =
  noEff Model {auctions = updatedAuctions, ..}
  where
    updatedAuctions = bidOnAuction auctionId bid auctions
updateModel (AppAction Login) Model {..} =
  Model {loggedIn = True, ..} <#
  pure (AppAction (SendMessage (Message username)))
updateModel (AppAction (UpdateUserNameField newUsername)) Model {..} =
  noEff Model {username = newUsername, ..}
updateModel (AppAction Noop) model = noEff model
updateModel _ model = noEff model
