
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module SharedTypes where 

import Data.Aeson.Types
import Data.Time.Clock
import GHC.Generics

import  Blockchain.Fae.FrontEnd

-- id of tx which created coin
newtype CoinTXID =
  CoinTXID String
  deriving (Show, Eq, Generic, ToJSON, FromJSON, Ord)

newtype TXID =
  TXID String
  deriving (Show, Eq, Generic, ToJSON, FromJSON, Ord)
 -- private key for signing txs

newtype Key =
  Key String
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
  -- id of the tx which created auction

newtype AucTXID =
  AucTXID String
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON, Ord) -- Int represents the number of the argument that failed
  -- hash of the coin

data Auction = Auction
  { bids :: [Bid]
  , createdBy :: String
  , createdTimestamp :: UTCTime
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

data Bid = Bid
  { bidValue :: Int
  , bidder :: String
  , bidTimestamp :: UTCTime
  , isWinningBid :: Bool
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

data Msg
  = CreateAuctionRequest -- incoming
  | BidRequest AucTXID -- incoming
               Int
  | BidSubmitted AucTXID -- outgoing
                 Bid
  | AuctionCreated AucTXID
                   Auction -- outgoing
  | RequestCoins Int -- incoming 
  | CoinsGenerated Int -- outgoing 
  | ErrMsg Err
  deriving (Show, Generic, FromJSON, ToJSON)

data Err
  = PostTXErr PostTXError -- outgoing
  | NoCoins -- outgoing
   deriving (Show, Generic, FromJSON, ToJSON)

data PostTXError
  = TXFailed String
  | TXBodyFailed String
  | TXInputFailed Int
  deriving (Show, Eq, Generic, ToJSON, FromJSON)