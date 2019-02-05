{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (finally)
import Data.Foldable
import qualified Data.Map.Lazy as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import Prelude
import System.Process

import Server
import PostTX

-- before running the server we reset the starting auction params in the 
-- "Create" source file by just overwriting the possibly modified version
-- with the original. This means that the starting bid and maxBidCount for
-- auctions will always respectively be 1 and 4, even if running instances of 
-- the server have changed these values in the source file before now.
main :: IO ()
main = do
    callCommand "cp CreateOriginal.hs Create.hs"
    runServer