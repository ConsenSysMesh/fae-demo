{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

--{-# LANGUAGE DuplicateRecordFields #-}
module Types where

import Data.Aeson.Types
import Data.Map.Lazy (Map)
import Data.Monoid
import Data.Text (Text)
import Data.Time.Clock
import GHC.Generics
import qualified Network.WebSockets as WS
import Data.Aeson.Types
import Data.Map.Lazy (Map)
import Data.Map.Lazy
import Data.Monoid
import Data.Time.Clock
import GHC.Generics
import SharedTypes

data Client = Client
  { name :: Text
  , conn :: WS.Connection
  , wallet :: Wallet
  }

instance Show Client where
  show Client {..} = show name <> show wallet

data ServerState = ServerState
  { clients :: [Client]
  , auctions :: Map AucTXID Auction
  } deriving (Show)

instance Eq Client where
  Client {name = name1} == Client {name = name2} = name1 == name2

newtype Wallet =
  Wallet (Map CoinTXID Int) -- Int is balance for coin cache
  deriving (Show, Eq)


  