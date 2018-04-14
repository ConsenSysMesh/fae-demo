{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import Data.Aeson.Types
import Data.Text (Text)
import GHC.Generics
import qualified Network.WebSockets as WS

--We represent a client by their username and a `WS.Connection`. We will see how we
--obtain this `WS.Connection` later on.
newtype Client =
  Client (Text, WS.Connection)

data ServerState = ServerState
  { clients :: [Client]
  , auctions :: [Auction]
  }

instance Eq Client where
  (Client (x, _)) == (Client (y, _)) = x == y

instance ToJSON Client where
  toJSON (Client (name, _)) = toJSON $ show (name, name)

data AuctionCreated = AuctionCreated
  { createdBy :: String
  , initialValue :: Double
  , maxNumBids :: Int
  , createdTimestamp :: String
  } deriving (Show, Generic, FromJSON)

data Bid = Bid
  { bidValue :: String
  , bidder :: String
  , bidTimestamp :: String
  } deriving (Show, Generic, FromJSON)

data Action
  = AuctionCreatedAction AuctionCreated
  | BidAction Bid
  | IdAction -- Identity action represents a noop
  deriving (Show, Generic, FromJSON)

instance ToJSON Action where
  toJSON action = toJSON $ show action

instance ToJSON AuctionCreated where
  toJSON auctionCreated = toJSON $ show auctionCreated

instance ToJSON Bid where
  toJSON bid = toJSON $ show bid

instance ToJSON Id where
  toJSON id = toJSON $ show id

newtype Auction =
  Auction [Bid]
  deriving (Show, Generic, FromJSON)
