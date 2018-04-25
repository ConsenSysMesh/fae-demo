{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FaeTX.Outgoing where

import Control.Monad
import Data.List
import Data.Monoid
import FaeTX.Outgoing.FormatTXinput
import FaeTX.Outgoing.PostTX
import FaeTX.Outgoing.Types
import Prelude
import System.Process
import Text.Pretty.Simple (pPrint)

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

callContract :: AuctionContract -> IO String
callContract (GetCoin (Key (key))) =
  postTX $ getPostTXargs (GetCoinTXinput (Key (key)))

main :: IO ()
main = getCs >>= pPrint

getCs = callContract (GetCoin (Key "tom"))
