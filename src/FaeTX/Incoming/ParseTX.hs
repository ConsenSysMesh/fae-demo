{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FaeTX.Incoming.ParseTX where

import Control.Monad
import Data.Maybe
import Prelude
import System.Process
import Text.Pretty.Simple (pPrint)
import Text.Regex.PCRE

import FaeTX.Incoming.Types

txIDregex :: String
txIDregex = "(?<=Transaction\\W)(\\w|\\d)+" :: String

coinVersionRegex :: String
coinVersionRegex = "(?<=versions:\\s      )(\\w|\\d)+" :: String

coinSCIDregex :: String
coinSCIDregex = "(?<=input )(\\w|\\d)+" :: String

-----------------------------------------------------------------------------
-- Parsing Output of postTX.sh
-----------------------------------------------------------------------------
parseTXid :: String -> String
parseTXid str = str =~ txIDregex :: String

parseCoinVersion :: String -> Maybe String
parseCoinVersion str
  | result == "" = Nothing
  | otherwise = Just result
  where
    result = str =~ coinVersionRegex :: String

parseCoinSCID :: String -> Maybe String
parseCoinSCID str
  | result == "" = Nothing
  | otherwise = Just result
  where
    result = str =~ coinSCIDregex :: String

parseTXoutput txOut = undefined

-- make sure that dev environment provisioning gives postTX.sh executable permissions
--postTX :: [String] -> IO TXout
--postTX args = txOut >>= pPrint >> txOut >>= pure . parseTXoutput
--  where
--    txOut = readProcess "./contracts/postTX.sh" args []
getFakeArg :: Bool -> String
getFakeArg fake =
  if fake
    then "--fake"
    else ""
{-
getArgs ::
     Contract
  -> Maybe Key
  -> Maybe AucTXid
  -> Maybe CoinTXid
  -> Maybe CoinSCID
  -> Maybe CoinVersion
  -> Fake
  -> [(String)]
getArgs Bid key aucTXid coinTXid coinSCID coinVersion isFake =
  [getFakeArg isFake, "Bid"]
getArgs Create _ _ _ _ _ _ = ["Create"]
getArgs Withdraw key aucTXid coinTXid coinSCID coinVersion _ = ["Withdraw"]
getArgs GetCoin (Just key) _ _ _ _ _ = ["-e key=" ++ key, "GetCoin"]
getArgs GetMoreCoins key _ _ _ _ _ = ["GetMoreCoins"]


createAuction :: IO CreateAuctionTXout
createAuction =
  postTX (getArgs Create Nothing Nothing Nothing Nothing Nothing False)

getCoin :: Key -> IO GetCoinTXout
getCoin key =
  postTX (getArgs GetCoin (Just key) Nothing Nothing Nothing Nothing False)

main :: IO ()
main = do
  getCoin "tom" >>= pPrint
-}
