module FaeTX.Incoming.Types where

import FaeTX.Types

type IsWinningBid = Bool

data AuctionTXout
  = FakeBidTXout Key
                 TXID
                 AucTXID
                 CoinTXID
                 CoinSCID
                 CoinVersion
  | BidTXout TXID
             AucTXID
             CoinTXID
             CoinSCID
             CoinVersion
             IsWinningBid
  | CreateAuctionTXout TXID
  | WithdrawCoinTXout TXID
                      CoinTXID
  | GetCoinTXout TXID
  | GetMoreCoinsTXout TXID
  deriving (Show, Eq)
