module ClientMsg.Outgoing.Types where

import FaeTX.Types

data TX
  = FakeBidTX Key
              AucTXID
              CoinTXID
  | BidTX Key
          AucTXID
          CoinTXID
          CoinSCID
          CoinVersion
  | CreateAuctionTX Key
  | WithdrawTX Key
               AucTXID
  | GetCoinTX Key
  | GetMoreCoinsTX Key
