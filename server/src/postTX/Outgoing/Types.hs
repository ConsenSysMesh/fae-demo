module PostTX.Outgoing.Types where

import PostTX.Types
import SharedTypes (Key, AucTXID, CoinTXID)

data AuctionTXin =
  BidTXin Key AucTXID CoinTXID
  | CreateAuctionTXin Key
  | WithdrawTXin Key AucTXID
  | GetCoinTXin Key
  | GetMoreCoinsTXin Key CoinTXID
  | CollectTXin Key AucTXID
  deriving (Show, Eq)
