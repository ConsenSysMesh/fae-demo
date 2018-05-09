{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module PostTX.Types where

import SharedTypes (Key, AucTXID, CoinTXID, TXID, PostTXError)

newtype CoinSCID =
  CoinSCID String
  deriving (Show, Eq)

newtype CoinVersion =
  CoinVersion String
  deriving (Show, Eq)

data PostTXResponse
  = AuctionCreatedTX TXID
  | FakeBidTX Key
            AucTXID
            CoinTXID
            CoinSCID
            CoinVersion
  | BidTX TXID
        AucTXID
        CoinTXID
        Bool
  | GetCoinTX TXID
  | GetMoreCoinsTX TXID
  | WithdrawTX TXID
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
