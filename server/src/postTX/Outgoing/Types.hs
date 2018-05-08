module PostTX.Outgoing.Types where

import PostTX.Types

data AuctionTXin
  = FakeBidTXin Key
                AucTXID
                CoinTXID
  | BidTXin Key
            AucTXID
            CoinTXID
            CoinSCID
            CoinVersion
  | CreateAuctionTXin Key
  | WithdrawTXin Key
                 AucTXID
  | GetCoinTXin Key
  | GetMoreCoinsTXin Key
                     CoinTXID
  deriving (Show, Eq)
