{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FaeTX.Incoming.ParseTX where

import Control.Monad
import Data.Maybe
import Prelude
import System.Process
import Text.Pretty.Simple (pPrint)
import Text.Regex.PCRE

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

parse = undefined
