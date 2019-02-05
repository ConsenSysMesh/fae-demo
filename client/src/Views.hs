{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Views where

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
import CreateAuctionView

import Data.Proxy
import Servant.API

import SharedViews


appView :: Model -> View Action
appView m = either (const the404) id $ runRoute (Proxy :: Proxy API) handlers uri m
  where
  handlers = login :<|> auctionHome :<|> create
  auctionHome (_ :: Model) = div_ [] [
     biddingView m 
    ]
  login (_ :: Model) = div_ [] [
     loginView m
    ]
  create (_ :: Model) = div_ [] [
     createAuctionView m
    ]
  the404 = div_ [] [
      text "404 :("
    , button_ [ onClick (AppAction goLogin) ] [ text "Login" ]
    ]

loginView :: Model -> View Action
loginView m@Model {..} = div_ [] [
  h1_ [class_ "heading"] [ text "Fae Auction"]
  , loginForm m
  ]

biddingView :: Model -> View Action
biddingView m@Model {..} =
  div_ [] $
  [ div_
      [class_ "main-container"]
      [headerView m, mainView m ]
  ]



-- container for the main auction content
mainView :: Model -> View Action
mainView Model {..} =
  case selectedAuctionTXID of
    Nothing -> ""
    (Just aucTXID) ->
      case M.lookup aucTXID auctions of
        Nothing -> ""
        (Just auction) ->
          auctionView maxBidCountField auctionStartValField aucTXID auction bidFieldValue

onEnter :: Action -> Attribute Action
onEnter action = onKeyDown $ bool (AppAction Noop) action . (== KeyCode 13)

viewAuction :: AucTXID -> Auction -> View Action
viewAuction aucTXID auction@Auction {..} =
  li_
    [class_ "auction-item", onClick $ AppAction (SelectAuction aucTXID)]
    [auctionText]
  where
    auctionText = p_ [] [text $ S.pack $ show auction]

getTableHeader :: [String] -> [View Action]
getTableHeader names =
  Prelude.map (\name -> th_ [] [p_ [] [text $ S.ms name]]) names

iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%H:%M:%S"

--use reader monad for selectedAuctionTXID
getTableRow :: Maybe AucTXID -> (AucTXID, Auction) -> View Action
getTableRow selectedAuctionTXID (txid@(AucTXID aucTXID), auction@Auction {..}) =
  tr_
    [ onClick $ AppAction (SelectAuction txid)
    , class_ $ do bool "auction-row" "auction-row-selected" isSelected
    ]
    [ td_ [] [text $ S.ms truncatedAucTXID]
    , td_ [] [text $ S.ms createdBy]
    , td_ [] [text $ S.ms $ iso8601 createdTimestamp]
    , td_ [] [text $ S.ms numBids]
    , td_ [] [text $ S.ms $ currentBidValue auction]
    , td_ [] [text $ S.ms $ auctionStatus auction]
    ]
  where
    isSelected = maybe False (txid ==) selectedAuctionTXID
    numBids = Li.length bids
    truncatedAucTXID = Li.take 5 aucTXID 

getTableRows :: Maybe AucTXID -> Map AucTXID Auction -> [View Action]
getTableRows selectedAucTXID auctions =
  Li.map (getTableRow selectedAucTXID) $ M.toList auctions

   -- table m | not $ Li.null txLog
txLogTable :: Model -> View Action
txLogTable Model {..} = table
  where table =  div_
            [class_ "tx-log-table"]
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
                , tbody_ [] $ getTableRows selectedAuctionTXID auctions
                ]
            ]


auctionView :: Int -> Int -> AucTXID -> Auction -> Int ->  View Action
auctionView maxBidCountField startingVal aucTXID@(AucTXID txid) auction@Auction {..} bidFieldValue = 
  div_ [class_ "auction-container"] [ 
    auctionViewLeft currentPrice auction, 
    auctionViewRight maxBidCountField aucTXID (bidFieldValue) auction]
  where currentPrice = if Li.null bids then S.ms startingVal else S.ms $ currentBidValue auction

auctionViewLeft currentPrice auction@Auction{..} =
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
            h3_ [] [text $ "Current Price  $ " <> (currentPrice)]
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
      ,
      div_
          [ class_ "auction-container-item book-description"]
          [
            h3_ [] [text "Fae Transaction Log"]
          ]
      ]


auctionViewRight maxBidCountField aucTXID bidFieldValue auction@Auction{..} =
  div_
    [class_ "auction-container-right"]
      [ div_
          [ class_ "auction-container-item"]
          [
            bookImg
          ]
      ,
      div_
        [class_ "bid-input-container auction-container-item"]
        [
            div_ 
              [class_ "bid-input-container"] 
              [ --h3_ [class_ "dollar"] [text "$"]
               -- , 
                input_
                [ class_ "field-input"
                , type_ "text"
                , placeholder_ "Enter Bid"
                , value_ $ S.pack $ show bidFieldValue
                , name_ "bidValue"
                , onInput $
                  AppAction . UpdateBidField . readMaybe . S.unpack . S.toMisoString
                ]
              ]
        ]
      ,
       div_
          [ class_ "auction-container-item"]
          [
            button_ [class_ "bid-field-btn", onClick (AppAction $ MintCoinsAndBid aucTXID (bidFieldValue)),
            disabled_ $ auctionEnded auction] [text "Submit Bid"]
          ]
      ,
      div_
          [ class_ "bid-progress-container"]
          [
            h3_ [] [text $ ("Bids: " <> (S.ms $ show $ Li.length bids)) <> "/" <> (S.ms $ show maxBidCountField)]
          ]
      
      ]
--
       --     h3_ [] [text $ "Auction " <> (S.pack $ show truncatedAucTXID)],
       --     div_ [class_ "auction-info"] [ h3_ [class_ "book_title"] [
       --       text $ S.pack "An Inquiry into the Nature and Causes of the Wealth of Nations. Smith, Adam. 1776"], p_ [class_ "book-description"] [text description], bookImg]
       --   ]
      --]
  --     ++
  --    [ footer_ [] [placeBidView aucTXID auction bidFieldValue username]
  --    | not $ auctionEnded auction
  --    ]
  --  ]
  --where
  --  bidValue = currentBidValue auction
  --  truncatedAucTXID = Li.take 5 txid

placeBidView :: AucTXID -> Auction -> Int -> String -> View Action
placeBidView aucTXID auction@Auction {..} bidFieldValue username =
  div_
    [class_ "place-bid-container"]
    [ input_
        [ class_ "field-input"
        , type_ "text"
        , placeholder_ "Enter Bid"
        , value_ $ S.pack $ show bidFieldValue
        , name_ "bidValue"
        , onInput $
          AppAction . UpdateBidField . readMaybe . S.unpack . S.toMisoString
        , onEnter bidAction
        ]
    , button_ [class_ "bid-field-btn", onClick bidAction, disabled_ $ auctionEnded auction] [text "Submit Bid"]
    ]
  where
    title = S.pack $ "Current Bid" ++ show aucTXID
    bidAction = AppAction $ MintCoinsAndBid aucTXID bidFieldValue

loginForm :: Model -> View Action
loginForm Model {..} =
  div_
    [class_ "login-form"]
    [ header_
        [class_ "header"]
        [
         input_
            [ type_ "text"
            , class_ "login-form-input"
            , onInput (AppAction . UpdateUserNameField . S.toMisoString)
            , onEnter (AppAction (Login))
            ]
        , button_
            [class_ "login-form-btn", onClick (AppAction (Login))]
            [text "Login"]
        ]
    ]
