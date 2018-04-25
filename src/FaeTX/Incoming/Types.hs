module FaeTX.Incoming.Types where

import FaeTX.Types

data AuctionTXout
  = FakeBidTXout TXID 
  AucTXID
  CoinTXID
  CoinSCID
  CoinVersion
  | BidTXout TXID
             AucTXID
             CoinTXID
  | CreateAuctionTXout TXID
                       AucTXID
  | WithdrawCoinTXout TXID
                      CoinTXID
  | GetCoinTXout TXID
                 CoinTXID
  | GetMoreCoinsTXout TXID
                      CoinTXID
  deriving (Show, Eq)
