{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}


module Types where

import GHC.Generics
import Data.Aeson.Types
import Data.Text (Text)
import qualified Network.WebSockets as WS


data Bid = Bid {
    value :: String
  , bidder :: String
  , timestamp :: String
  } | Noop deriving (Show, Generic)
  
instance FromJSON Bid
instance ToJSON Bid where
    toJSON bid = toJSON $ show bid

--We represent a client by their username and a `WS.Connection`. We will see how we
--obtain this `WS.Connection` later on.

newtype Client = Client (Text, WS.Connection)

instance Eq Client where
  (Client (x, _)) == (Client (y, _)) = x == y

--The state kept on the server is simply a list of connected clients. We've added
--an alias and some utility functions, so it will be easier to extend this state
--later on.

type ServerState = [Client]

instance ToJSON Client where
    -- No need to provide a toJSON implementation.

    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toJSON (Client (name, _)) = toJSON $ show (name, name)
