{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module FaeTX.Types where

import Data.Aeson.Types
import GHC.Generics

newtype TXID =
  TXID String
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
 -- private key for signing txs

newtype Key =
  Key String
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- hash of the coin
newtype CoinSCID =
  CoinSCID String
  deriving (Show, Eq)

newtype CoinVersion =
  CoinVersion String
  deriving (Show, Eq)

-- id of tx which created coin
newtype CoinTXID =
  CoinTXID String
  deriving (Show, Eq, Generic, ToJSON, FromJSON, Ord)

-- id of the tx which created auction
newtype AucTXID =
  AucTXID String
  deriving (Show, Eq, Generic, ToJSON, FromJSON) -- Int represents the number of the argument that failed

data PostTXError
  = TXFailed String
  | TXBodyFailed String
  | TXInputFailed Int
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data PostTXResponse
  = CreateAuction TXID
  | FakeBid Key
            AucTXID
            CoinTXID
            CoinSCID
            CoinVersion
  | Bid TXID
        AucTXID
        Bool
  | GetCoin TXID
  | GetMoreCoins TXID
  | Withdraw TXID
  deriving (Show, Eq)

data TXConfig
  = BidConfig Key
              AucTXID
              CoinTXID
  | CreateAuctionConfig Key
  | GetCoinConfig Key
  | GetMoreCoinsConfig Key
                       CoinTXID
  | WithdrawConfig Key
                   AucTXID
