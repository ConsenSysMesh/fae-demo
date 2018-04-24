{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ClientMsg.Outgoing where

import ClientMsg.Types
import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Foldable
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as X
import qualified Data.Text.Lazy.Encoding as D
import qualified Network.WebSockets as WS
import Prelude
import Text.Pretty.Simple (pPrint)

import Auction
import Types

encodeAuctionAction :: ClientMsg.Types.AuctionAction -> Text
encodeAuctionAction a = T.pack $ show $ X.toStrict $ D.decodeUtf8 $ encode a
