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
  deriving (Show, Eq) -- Int represents the number of the argument that failed

data PostTXError
  = TXFailed String
  | TXBodyFailed String
  | TXInputFailed Int

data PostTXResponse
  = CreateAuction TXID
  | FakeBid Key
            AucTXID
            CoinTXID
            CoinSCID
            CoinVersion
  | Bid TXID
        AucTXID
        Bool
  | GetCoin TXID
  | GetMoreCoins TXID
  | Withdraw TXID
  deriving (Show, Eq)
