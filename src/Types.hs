{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

--{-# LANGUAGE DuplicateRecordFields #-}
module Types where

import Data.Aeson.Types
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Text (Text)
import Data.Time.Clock
import GHC.Generics
import qualified Network.WebSockets as WS

newtype Client = Client { 
   name ::Text
 , conn :: Ws.Connection
   , wallet :: Wallet
}

newtype Key =
  Key String

newtype AuctionID =
  AuctionID String

instance Show Client where
  show (Client (name, _)) = show name

data ServerState = ServerState
  { clients :: [Client]
  , auctions :: Map String Auction
  } deriving (Show)

data Auction = Auction
  { auctionId :: String
  , bids :: [Bid]
  , createdBy :: String
  , initialValue :: Int
  , maxNumBids :: Int
  , createdTimestamp :: UTCTime
  } deriving (Show, Generic, FromJSON, ToJSON)

data Msg
  = CreateAuction
  | Bid AuctionID
        Int
  | RequestCoins Int
  deriving (Show, Generic, FromJSON, ToJSON)

data Bid = Bid
  { bidValue :: Int
  , bidder :: String
  , bidTimestamp :: UTCTime
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Eq Client where
  (Client (x, _)) == (Client (y, _)) = x == y
  -- Actions for synchronising client-server state

newtype CoinCacheID =
  CoinCacheID String

newtype Wallet =
  Wallet (Map CoinCacheID Int)
