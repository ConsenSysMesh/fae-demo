{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FaeTX.Incoming.ParseTX where

import Control.Monad
import Data.Maybe
import FaeTX.Incoming.Types
import FaeTX.Types
import Prelude
import Text.Pretty.Simple (pPrint)
import Text.Regex.PCRE

txidRegex :: String
txidRegex = "(?<=Transaction\\W)(\\w|\\d)+" :: String

coinVersionRegex :: String
coinVersionRegex = "(?<=versions:\\s      )(\\w|\\d)+" :: String

coinSCIDregex :: String
coinSCIDregex = "(?<=input )(\\w|\\d)+" :: String

txidParser :: String -> Maybe String
txidParser = runRegex txidRegex -- case runRegex txidRegex of

coinVersionParser :: String -> Maybe String
coinVersionParser = runRegex coinVersionRegex

coinSCIDparser :: String -> Maybe String
coinSCIDparser = runRegex coinSCIDregex

runRegex :: String -> String -> Maybe String
runRegex regex str
  | result == "" = Nothing
  | otherwise = (Just result)
  where
    result = str =~ regex :: String

fakeBidParser :: AucTXID -> CoinTXID -> String -> Maybe AuctionTXout
fakeBidParser aucTXID coinTXID txOut =
  coinSCIDparser txOut >>= \coinSCID ->
    coinVersionParser txOut >>= \coinVersion ->
      txidParser txOut >>= \txid ->
        return
          (FakeBidTXout
             (TXID txid)
             aucTXID
             coinTXID
             (CoinSCID coinSCID)
             (CoinVersion coinVersion))
