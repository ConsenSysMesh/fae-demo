{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import Data.Aeson.Types
import Data.Text (Text)
import GHC.Generics
import qualified Network.WebSockets as WS

--We represent a client by their username and a `WS.Connection`. We will see how we
--obtain this `WS.Connection` later on.
newtype Client =
  Client (Text, WS.Connection)

data ServerState = ServerState
  { clients :: [Client]
  , auctions :: [String]
  }

instance Eq Client where
  (Client (x, _)) == (Client (y, _)) = x == y

instance ToJSON Client where
  toJSON (Client (name, _)) = toJSON $ show (name, name)

data Bid
  = Bid { value :: String
        , bidder :: String
        , timestamp :: String }
  | Noop
  deriving (Show, Generic)

instance FromJSON Bid

instance ToJSON Bid where
  toJSON bid = toJSON $ show bid
