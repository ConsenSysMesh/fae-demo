import FaeTX.Types

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
    | WithdrawCoinTXin Key
                     AucTXID
    | GetCoinTXin Key
    | GetMoreCoinsTXin Key
                     CoinTXID
    deriving (Show, Eq)