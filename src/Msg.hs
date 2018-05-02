{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

---------------------------------------------------------------------------
-- Post Transactions and Sync Server state and Broadcast msg to clients
---------------------------------------------------------------------------
module Msg 
  ( msgHandler
  ) where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (finally)
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Foldable
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as X
import qualified Data.Text.Lazy.Encoding as D
import FaeTX.Post
import FaeTX.Types (Key, CoinTXID, AucTXID)
import Clients
import qualified Network.WebSockets as WS
import Prelude
import Text.Pretty.Simple (pPrint)
import Types
import Auction
import Coins

msgHandler :: Msg -> Client -> ServerState -> IO a
msgHandler CreateAuctionMsg Client{..} ServerState{..} = undefined
--msgHandler (BidMsg aucId amount) Client{..} ServerState {..} = undefined
 --    where  key = "bidder1"
 --           postTXResult = bid key aucId amount
--msgHandler (RequestCoinsMsg numCoins) Client{..} ServerState {..} = addCoinsToWallet key clientWallet numCoins
--     where  key = "bidder1"
--            clientWallet = getClientWallet

