{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{----------------------------------------------
  Api for High level fae auction TXs management
-----------------------------------------------}
module FaeTX.Main where

import Control.Monad
import Data.List
import Data.Monoid
import FaeTX.Outgoing.FormatTX
import FaeTX.Outgoing.PostTX
import Prelude
import System.Process
import Text.Pretty.Simple (pPrint)

import FaeTX.Types

callContract :: AuctionTX -> IO String
callContract (GetCoinTX key) = postTX (GetCoinTX key)
callContract (CreateAuctionTX key) = postTX (CreateAuctionTX key)
callContract (WithdrawCoinTX key aucTXID) = postTX (WithdrawCoinTX key aucTXID)

main :: IO ()
main = getCs >>= pPrint

getCs =
  callContract
    (WithdrawCoinTX
       (Key "tom")
       (AucTXID
          "d32918fbcd3eebdcc37bd0271b0033868c36ac8695078187538423b22a03cdac"))
