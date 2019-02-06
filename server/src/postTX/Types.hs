{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module PostTX.Types where

import SharedTypes (Key, AucTXID, CoinTXID, PostTXError)
import FaeFrontend

newtype AucStartingValue = AucStartingValue Int deriving (Show, Eq)

data PostTXResponse
  = AuctionCreatedTX TransactionID AucStartingValue
  | BidTX TransactionID
        AucTXID
        CoinTXID
        Bool
  | GetCoinTX TransactionID
  | GetMoreCoinsTX TransactionID
  | WithdrawTX TransactionID
  deriving (Show, Eq)

data TXConfig
  = BidConfig Key
              AucTXID
              CoinTXID
  | CreateAuctionConfig Key AucStartingValue
  | GetCoinConfig Key
  | GetMoreCoinsConfig Key
                       CoinTXID
  | WithdrawConfig Key
                   AucTXID
