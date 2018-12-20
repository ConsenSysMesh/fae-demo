{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module PostTX.Types where

import SharedTypes (Key, AucTXID, CoinTXID, PostTXError)
import FaeFrontend

data PostTXResponse
  = AuctionCreatedTX TransactionID
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
  | CreateAuctionConfig Key
  | GetCoinConfig Key
  | GetMoreCoinsConfig Key
                       CoinTXID
  | WithdrawConfig Key
                   AucTXID
