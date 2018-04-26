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
import Debug.Trace

txidRegex :: String
txidRegex = "(?<=Transaction\\W)(\\w|\\d)+" :: String

coinVersionRegex :: String
coinVersionRegex = "(?<=versions:\\s      )(\\w|\\d)+" :: String

coinSCIDregex :: String
coinSCIDregex = "(?<=input )(\\w|\\d)+" :: String

hasWonRegex :: String
hasWonRegex = "You won!" :: String

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

hasWonParser :: String -> Maybe String
hasWonParser = runRegex hasWonRegex

hasWonAuction :: String -> Bool
hasWonAuction txOut = isJust $ hasWonParser txOut

createAuctionParser :: String -> Maybe AuctionTXout
createAuctionParser txOut = case txidParser txOut of 
  Just txid -> Just (CreateAuctionTXout (TXID txid))
  Nothing -> Nothing

getCoinParser :: String -> Maybe AuctionTXout
getCoinParser txOut = case txidParser txOut of 
  Just txid -> Just (GetCoinTXout (TXID txid))
  Nothing -> Nothing

fakeBidParser :: Key -> AucTXID -> CoinTXID -> String -> Maybe AuctionTXout
fakeBidParser key aucTXID coinTXID txOut = do
  coinSCID <- coinSCIDparser txOut
  coinVersion <- coinVersionParser txOut 
  txid <- txidParser txOut
  return (FakeBidTXout
             key
             (TXID txid)
             aucTXID
             coinTXID
             (CoinSCID coinSCID)
             (CoinVersion coinVersion))

bidParser :: Key -> AucTXID -> CoinTXID -> String -> Maybe AuctionTXout
bidParser key aucTXID coinTXID txOut =
  coinSCIDparser txOut >>= \coinSCID ->
    coinVersionParser txOut >>= \coinVersion ->
      txidParser txOut >>= \txid ->
        return
          (BidTXout
             (TXID txid)
             aucTXID
             coinTXID
             (CoinSCID coinSCID)
             (CoinVersion coinVersion)
             (hasWonAuction txOut))
