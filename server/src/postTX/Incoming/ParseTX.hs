{-# LANGUAGE OverloadedStrings #-}

module PostTX.Incoming.ParseTX where

import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import PostTX.Incoming.Types
import Prelude
import Text.Regex.PCRE
import PostTX.Types

import SharedTypes

exceptionRegex :: String
exceptionRegex = "<exception>" :: String

txidRegex :: String
txidRegex = "(?<=Transaction\\W)(\\w|\\d)+" :: String

coinVersionRegex :: String
coinVersionRegex = "(?<=versions:\\s      )(\\w|\\d)+" :: String

coinSCIDregex :: String
coinSCIDregex = "(?<=input )(\\w|\\d)+" :: String

hasWonRegex :: String
hasWonRegex = "You won!" :: String

txidParser :: String -> Maybe String
txidParser = runRegex txidRegex

coinVersionParser :: String -> Maybe String
coinVersionParser = runRegex coinVersionRegex

coinSCIDparser :: String -> Maybe String
coinSCIDparser = runRegex coinSCIDregex

exceptionParser :: String -> Maybe String
exceptionParser = runRegex exceptionRegex

runRegex :: String -> String -> Maybe String -- change maybe string to maybe a
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
createAuctionParser txOut =
  case txidParser txOut of
    Just txid -> Just (CreateAuctionTXout (TXID txid))
    Nothing -> Nothing

getCoinParser :: String -> Maybe AuctionTXout
getCoinParser txOut =
  case txidParser txOut of
    Just txid -> Just (GetCoinTXout (TXID txid))
    Nothing -> Nothing

getMoreCoinsParser :: String -> Maybe AuctionTXout
getMoreCoinsParser txOut =
  txidParser txOut >>= \txid ->
    case exceptionParser txOut of
      (Just _) -> Nothing
      _ -> Just $ GetMoreCoinsTXout (TXID txid)

withdrawParser :: String -> Maybe AuctionTXout
withdrawParser txOut =
  case txidParser txOut of
    Just txid -> Just (WithdrawTXout (TXID txid))
    Nothing -> Nothing

-- fake bids postTX output has exceptions so don't use exception parser
fakeBidParser :: String -> ReaderT TXConfig Maybe FakeBidTXout
fakeBidParser txOut = do
  (BidConfig key aucTXID coinTXID) <- ask
  coinSCID <- lift $ coinSCIDparser txOut
  coinVersion <- lift $ coinVersionParser txOut
  txid <- lift $ txidParser txOut
  return
    (FakeBidTXout
       key
       (TXID txid)
       aucTXID
       coinTXID
       (CoinSCID coinSCID)
       (CoinVersion coinVersion))

bidParser :: String -> ReaderT TXConfig Maybe BidTXout
bidParser txOut = do
  (BidConfig key aucTXID coinTXID) <- ask
  exception <- lift $ exceptionParser txOut
  coinSCID <- lift $ coinSCIDparser txOut
  coinVersion <- lift $ coinVersionParser txOut
  coinSCID <- lift $ coinSCIDparser txOut
  txid <- lift $ txidParser txOut
  return
    (BidTXout
       (TXID txid)
       aucTXID
       coinTXID
       (CoinSCID coinSCID)
       (CoinVersion coinVersion)
       (hasWonAuction txOut))
