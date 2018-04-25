{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FaeTX.Outgoing.PostTX where

import Control.Monad
import Data.List
import Data.Monoid
import FaeTX.Outgoing.FormatTXinput
import FaeTX.Outgoing.Types
import Prelude
import System.Process
import Text.Pretty.Simple (pPrint)

-- make sure that dev environment provisioning gives postTX.sh executable permissions
postTX :: [String] -> IO String
postTX args = readProcess "./contracts/postTX.sh" args []
{-
main :: IO ()
main = getCs

getC = postTX $ getPostTXargs (GetCoinTXinput (Key "tom"))

coint =
  CoinTXID "02a3335a32793f66b102854591ec9614f75a5e7c216b0f24f4dfc5e51db62353"

auct =
  AucTXID "4109a5409412c8b6b9f54bf3f10eeae36d4885431dee90ea7442e410bd7dd273"

getCs = postTX $ getPostTXargs (GetMoreCoinsTXinput (Key "tom") (coint))
-}
