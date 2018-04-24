{-# LANGUAGE RecordWildCards #-}

module ClientMsg.Incoming where

import Auction
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
import Types

updateAuctions ::
     IntMap Auction -> ClientMsg.Types.AuctionAction -> IntMap Auction
updateAuctions auctions (ClientMsg.Types.CreateAuctionAction auc@Auction {..}) =
  createAuction auc auctions
updateAuctions auctions (ClientMsg.Types.BidAuctionAction auctionId bid) =
  bidOnAuction auctionId bid auctions

parseAuctionAction :: Text -> Maybe ClientMsg.Types.AuctionAction
parseAuctionAction jsonTxt = decode $ C.pack $ T.unpack jsonTxt

handleAuctionAction ::
     ServerState -> ClientMsg.Types.AuctionAction -> ServerState
handleAuctionAction ServerState {..} auctionAction =
  ServerState {auctions = updateAuctions auctions auctionAction, ..}
