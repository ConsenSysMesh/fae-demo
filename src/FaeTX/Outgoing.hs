module FaeTX.Outgoing where

import FaeTX.Types

data AuctionContract
  = Bid Key
        AucTXID
        CoinTXID
  | CreateAuction Key
  | Withdraw Key
             AucTXID
  | GetCoin Key
  | GetMoreCoins Key
  deriving (Show, Eq)

postTX :: AuctionContract -> String
postTX contract = undefined
