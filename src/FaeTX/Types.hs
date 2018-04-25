module FaeTX.Types where

newtype TXID =
  TXID String
  deriving (Show, Eq)
 -- private key for signing txs

newtype Key =
  Key String
  deriving (Show, Eq)

-- hash of the coin
newtype CoinSCID =
  CoinSCID String
  deriving (Show, Eq)

newtype CoinVersion =
  CoinVersion String
  deriving (Show, Eq)

-- id of tx which created coin
newtype CoinTXID =
  CoinTXID String
  deriving (Show, Eq)

-- id of the tx which created auction
newtype AucTXID =
  AucTXID String
  deriving (Show, Eq)

data AuctionContract
  = Create Key
  | Bid Key
        AucTXID
        CoinTXID
        CoinSCID
        CoinVersion
  | GetCoin Key
  | GetMoreCoins Key
                 CoinTXID
  | Withdraw Key
             AucTXID
  deriving (Show, Eq)
