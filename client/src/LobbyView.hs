
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module LobbyView where

import Auction
import Data.Aeson as A
import Data.Bool
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.JSString as GJS
import qualified Data.List as Li
import qualified Data.Map as M
import Data.Map.Lazy (Map)
import Data.Maybe
import Data.Monoid
import qualified Data.Text.Lazy as W
import Data.Text.Lazy.Encoding as X
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import GHC.Generics
import Miso
import Miso.String (MisoString)
import qualified Miso.String as S
import Text.Read
import Types
import Debug.Trace
import SharedTypes
import SharedViews

import Data.Proxy
import Servant.API

lobbyView :: Model -> View Action
lobbyView m@Model {..} =
  div_ [] $
  [ div_
      [class_ "main-container"]
      [headerView m, joinAuctionBtn, createAuctionBtn, lobbyTable m]
  ]

createAuctionBtn :: View Action
createAuctionBtn = button_ [ class_ "create-auction-btn", onClick (AppAction goCreate) ] [
        text "create auction"
      ]

joinAuctionBtn :: View Action
joinAuctionBtn = button_ [ class_ "join-auction-btn", onClick (AppAction goAuctionHome) ] [
        text "join auction"
      ]

lobbyTable :: Model -> View Action
lobbyTable Model {..} =
  div_
    [class_ "auctions-table"]
    [ table_
        []
        [ thead_
            [style_ $ M.fromList [(S.pack "text-align", S.pack "center")]]
            [ tr_
                []
                [ th_ [] [p_ [] [text "Auction ID"]]
                , th_ [] [p_ [] [text "Created By"]]
                , th_ [] [p_ [] [text "Created At"]]
                , th_ [] [p_ [] [text "Bids"]]
                , th_ [] [p_ [] [text "Current Bid"]]
                , th_ [] [p_ [] [text "Status"]]
                ]
            ]
        , tbody_ [] $ getLobbyTableRows selectedAuctionTXID auctions
        ]
    ]

getLobbyTableRows :: Maybe AucTXID -> Map AucTXID Auction -> [View Action]
getLobbyTableRows selectedAucTXID auctions = traceShow (M.elems auctions) tab
   where tab =   Li.map (getLobbyTableRow selectedAucTXID) $ M.toList auctions

getLobbyTableRow :: Maybe AucTXID -> (AucTXID, Auction) -> View Action
getLobbyTableRow selectedAuctionTXID (txid@(AucTXID aucTXID), auction@Auction {..}) =
  tr_
    [ onClick $ AppAction (SelectAuction txid)
    , class_ $ do bool "auction-row" "auction-row-selected" isSelected
    ]
    [ td_ [] [text $ S.ms truncatedAucTXID]
    , td_ [] [text $ S.ms createdBy]
    , td_ [] [text $ S.ms $ format24hrTime createdTimestamp]
    , td_ [] [text $ S.ms numBids]
    , td_ [] [text $ S.ms $ currentBidValue auction]
    , td_ [] [text $ S.ms $ auctionStatus auction]
    ]
  where
    isSelected = maybe False (txid ==) selectedAuctionTXID
    numBids = Li.length bids
    truncatedAucTXID = Li.take 5 aucTXID 
