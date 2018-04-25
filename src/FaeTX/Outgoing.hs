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
callContract (GetCoin key) =
  postTX $ getPostTXargs (GetCoinTXinput key)
callContract (CreateAuction key) =
  postTX $ getPostTXargs (CreateAuctionTXinput key)
callContract (Withdraw key aucTXID) =
  postTX $ getPostTXargs (WithdrawCoinTXinput key aucTXID)

main :: IO ()
main = getCs >>= pPrint

getCs = callContract (Withdraw (Key "tom") (AucTXID "d32918fbcd3eebdcc37bd0271b0033868c36ac8695078187538423b22a03cdac"))
