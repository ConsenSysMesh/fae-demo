{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

---------------------------------------------------------------------------
-- Post Transactions and Sync Server state and Broadcast msg to clients
---------------------------------------------------------------------------
module Msg where
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
import FaeTX.Types
  ( AucTXID
  , CoinTXID
  , Key
  , PostTXError
  , PostTXResponse
  , TXConfig
  )
import qualified Network.WebSockets as WS
import Prelude
import Text.Pretty.Simple (pPrint)
import Types
import Auction
import Coin

msgHandler :: Msg -> WS.Connection -> ServerState -> IO a
msgHandler (CreateAuction) Client{..} ServerState{..} =
  where postTXResult = createAuction Key
        key = 
msgHandler (Bid aucId amount) Client{..} ServerState {..} = 
     where postTXResult = bid key aucId amount
           key = "bidder1"
msgHandler (RequestCoins numCoins) Client{..} ServerState {..} = addCoinsToWallet key clientWallet amount
     where  postTXResult = bid key aucId amount
            key = "bidder1"

callContract :: TXConfig -> IO (Either PostTXError PostTXResponse)
callContract = executeContract

-- given a previous cache returns a new cache with requested coins
getCoins :: Int -> Key -> CoinTXID -> IO (Either PostTXError PostTXResponse)
getCoins numCoins key coinCache = undefined

bid = undefined

createAuction = undefined