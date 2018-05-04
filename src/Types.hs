{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

--{-# LANGUAGE DuplicateRecordFields #-}
module Types where

import Data.Aeson.Types
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Monoid
import Data.Text (Text)
import Data.Time.Clock
import FaeTX.Post
import GHC.Generics
import qualified Network.WebSockets as WS

data Client = Client
  { name :: Text
  , conn :: WS.Connection
  , wallet :: Wallet
  }

instance Show Client where
  show Client {..} = show name <> show wallet

data ServerState = ServerState
  { clients :: [Client]
  , auctions :: Map AucTXID Auction
  } deriving (Show)

data Auction = Auction
  { auctionId :: AucTXID
  , bids :: [Bid]
  , createdBy :: String
  , initialValue :: Int
  , maxNumBids :: Int
  , createdTimestamp :: UTCTime
  } deriving (Show, Generic, FromJSON, ToJSON)

data Msg
  = CreateAuctionMsg
  | BidMsg AucTXID
           Int
  | RequestCoinsMsg Int
  | CoinsGeneratedMsg Int
  | ErrMsg PostTXError
  deriving (Show, Generic, FromJSON, ToJSON)

data Bid = Bid
  { bidValue :: Int
  , bidder :: String
  , bidTimestamp :: UTCTime
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Eq Client where
  Client {name = name1} == Client {name = name2} = name1 == name2

newtype Wallet =
  Wallet (Map CoinTXID Int) -- Int is balance for coin cache
  deriving (Show, Eq)
