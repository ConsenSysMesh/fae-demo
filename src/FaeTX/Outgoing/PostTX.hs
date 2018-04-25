{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FaeTX.Outgoing.PostTX where

import Control.Monad
import Data.List
import Data.Monoid
import FaeTX.Outgoing.FormatTX
import FaeTX.Incoming.Types
import FaeTX.Types
import Prelude
import System.Process
import Text.Pretty.Simple (pPrint)

-- make sure that dev environment provisioning gives postTX.sh executable permissions
postTX :: AuctionTXin -> IO String
postTX tx = readProcess "./contracts/postTX.sh" args []
  where
    args = getPostTXargs tx
