{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module CreateAuctionView where

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
import SharedTypes

import Data.Proxy
import Servant.API

import SharedViews

createAuctionView :: Model -> View Action
createAuctionView m@Model {..} = div_ [] [
    headerView m,
    createAuctionContainerView auctionStartValField maxBidCountField  
    ]


createAuctionContainerView startingValField maxBidCountField = 
 div_ [
       class_ "auction-container"
      ] 
      [ 
        createAuctionViewLeft, 
        createAuctionViewRight startingValField maxBidCountField
      ]

createAuctionBtn :: View Action
createAuctionBtn =
  div_ [ class_ "create-auction-container" ] $
    [button_
      [ 
          onClick (AppAction SendCreateAuctionRequest)
      ]
      [text "Create New Auction"]
      ]

createAuctionViewLeft  =
  div_
    [class_ "auction-container-left"]
      [ div_
          [ class_ "auction-container-item no-margin-top"]
          [
            h3_ [] [text "Seller London Book House"]
          ]
      ,
      div_
        [class_ "auction-container-item no-margin-top"]
        [
            h3_ [] [text "Item 58113861094"]
        ]
      ,
      div_
          [ class_ "auction-container-item no-margin-top"]
          [
            h3_ [] [text "Description"]
          ]
      ,
      div_
          [ class_ "auction-container-item no-margin-top"]
          [
            p_ [] [text description]
          ]
      ]


createAuctionViewRight startingValField maxBidCountField =
  div_
    [class_ "auction-container-right"]
      [ div_
          [ class_ "auction-container-item"]
          [
            bookImg
          ]
      ,
      div_
        [class_ "starting-val-container auction-container-item"]
        [
               --h3_ [class_ "dollar"] [text "$"]
               -- , 
                h3_ [class_ "starting-price-label"] [text "Starting Price"] 
                , input_
                [ class_ "create-field-input"
                , type_ "text"
                , placeholder_ "Enter Bid"
                , value_ $ S.pack $ show startingValField
                , name_ "UpdateNewStartingValField"
                , onInput $
                  AppAction . UpdateNewStartingValField . read . S.unpack . S.toMisoString
                ]
              ]
      ,
      div_ 
            [class_ "max-bid-container"] 
            [ --h3_ [class_ "dollar"] [text "$"]
               -- ,
              h3_ [class_ "max-bid-label"] [text "Max Bids"]
              , input_
                [ class_ "create-field-input"
                , type_ "text"
                , placeholder_ "Enter Bid"
                , value_ $ S.pack $ show maxBidCountField
                , name_ "UpdateNewMaxBidCountField"
                , onInput $
                  AppAction . UpdateNewMaxBidCountField . read . S.unpack . S.toMisoString
                ]
            ]
      ,
      createAuctionBtn
      ]