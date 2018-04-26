{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FaeTX.Outgoing.PostTX where

import Control.Monad
import Data.List
import Data.Monoid
import FaeTX.Incoming.Types
import FaeTX.Outgoing.FormatTX
import FaeTX.Outgoing.Types
import FaeTX.Types
import System.Process
import System.IO

postTX :: AuctionTXin -> IO String
postTX tx = do
    postTXoutput <- readProcess path args []
    System.IO.putStrLn postTXoutput
    return postTXoutput
    where args = getPostTXargs tx 
          path = "./contracts/postTX.sh"
