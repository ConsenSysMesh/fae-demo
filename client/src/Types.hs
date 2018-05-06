{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Types where

import Data.Aeson as A
import Data.Bool
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.IntMap.Lazy as IntMap
import Data.IntMap.Lazy (IntMap)
import qualified Data.JSString as GJS
import qualified Data.Map as M
import Data.Monoid
import Data.Time.Clock
import GHC.Generics
import Miso
import Miso.String (MisoString)
import qualified Miso.String as S
import Miso.Subscription.WebSocket

newtype Message =
  Message MisoString
  deriving (Eq, Show, Generic)

instance ToJSON Message

instance FromJSON Message where
  parseJSON =
    withText "Not a valid string" $ \x -> pure (Message (S.toMisoString x))

data Model = Model
  { msg :: Message
  , received :: MisoString
  , auctions :: IntMap Auction
  , bidFieldValue :: Int
  , username :: MisoString
  , loggedIn :: Bool
  , selectedAuctionId :: AuctionId
  } deriving (Show, Eq)

data Action
  = AppAction AppAction
  | AuctionAction AuctionAction -- incoming ws actions

-- Actions for updating local client state
data AppAction
  = HandleWebSocket (WebSocket Message)
  | SendMessage Message
  | SendAuctionAction AuctionAction
  | UpdateUserNameField MisoString
  | Login
  | UpdateMessage MisoString
  | UpdateBidField (Maybe Int)
  | SelectAuction AuctionId
  | Noop

type AuctionId = Int

data Auction = Auction
  { auctionId :: AuctionId
  , bids :: [Bid]
  , createdBy :: String
  , initialValue :: Int
  , maxNumBids :: Int
  , createdTimestamp :: UTCTime
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

data Bid = Bid
  { bidValue :: Int
  , bidder :: String
  , bidTimestamp :: UTCTime
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- Shared actions with server
data AuctionAction
  = CreateAuctionAction Auction
  | BidAuctionAction AuctionId
                     Bid
  deriving (Eq, Show, Generic, FromJSON, ToJSON)
