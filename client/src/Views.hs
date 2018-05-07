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

appView :: Model -> View Action
appView m@Model {..} =
  div_ [style_ $ M.fromList [("text-align", "center")]] $
  [h1_ [class_ "title"] [text "Fae Auction"]] ++
  [img_ [src_ "background.jpg"]] ++
  [div_ [class_ $ bool "visible" "hidden" loggedIn] [loginForm m]] ++
  [ div_
      [class_ $ "auctions-table-container " <> bool "hidden" "visible" loggedIn]
      auctionViews
  ]
  where
    auctionViews =
      [createAuctionView m] ++
      [viewAuctionsTable m | not $ M.null auctions] ++ [selectedAuctionView m]

selectedAuctionView :: Model -> View Action
selectedAuctionView Model {..} =
  case selectedAuctionTXID of
    Nothing -> ""
    (Just aucTXID) ->
      case M.lookup aucTXID auctions of
        Nothing -> ""
        (Just auction) ->
          auctionView aucTXID auction bidFieldValue (S.fromMisoString username)
          -- TODO dont pass auction down instead use reader to pass down auction state and use selected auction id to get auction data

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
getTableRow selectedAuctionTXID (aucTXID, auction@Auction {..}) =
  tr_
    [ onClick $ AppAction (SelectAuction aucTXID)
    , class_ $ do bool "auction-row" "auction-row-selected" isSelected
    ]
    [ td_ [] [text $ S.ms aucTXID]
    , td_ [] [text $ S.ms createdBy]
    , td_ [] [text $ S.ms $ iso8601 createdTimestamp]
    , td_ [] [text $ S.ms numBids]
    , td_ [] [text $ S.ms $ currentBidValue auction]
    , td_ [] [text $ S.ms $ auctionStatus auction]
    ]
  where
    isSelected = maybe False (aucTXID ==) selectedAuctionTXID
    numBids = Li.length bids

getTableRows :: Maybe AucTXID -> Map AucTXID Auction -> [View Action]
getTableRows selectedAucTXID auctions =
  Li.map (getTableRow selectedAucTXID) $ M.toList auctions

viewAuctionsTable :: Model -> View Action
viewAuctionsTable Model {..} =
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
        , tbody_ [] $ getTableRows selectedAuctionTXID auctions
        ]
    ]

createAuctionView :: Model -> View Action
createAuctionView Model {..} =
  button_
    [ style_ $ M.singleton "font-size" "1.25em"
    , onClick (AppAction (SendAuctionAction (CreateAuctionRequest)))
    ]
    [text "Create New Auction"]

auctionView :: AucTXID -> Auction -> Int -> String -> View Action
auctionView aucTXID auction@Auction {..} bidFieldValue username =
  div_
    [class_ "auction-view"]
    [ article_ [class_ "card"] $
      [ header_
          [ class_ "header"
          , style_ $ M.fromList [(S.pack "text-align", S.pack "center")]
          ]
          [h3_ [] [text $ "Auction " <> (S.pack $ show aucTXID)]]
      ] ++
      [ footer_ [] [placeBidView aucTXID auction bidFieldValue username]
      | not auctionEnded
      ]
    ]
  where
    auctionEnded = False -- TODO REMOVE HARDCODING

placeBidView :: AucTXID -> Auction -> Int -> String -> View Action
placeBidView aucTXID auction@Auction {..} bidFieldValue username =
  div_
    [class_ "place-bid-container"]
    [ input_
        [ class_ "bid-field-input"
        , type_ "text"
        , placeholder_ "Enter Bid"
        , autofocus_ True
        , value_ $ S.pack $ show bidFieldValue
        , name_ "bidValue"
        , onInput $
          AppAction . UpdateBidField . readMaybe . S.unpack . S.toMisoString
        , onEnter bidAction
        ]
    , button_ [class_ "bid-field-btn", onClick bidAction] [text "Place Bid"]
    ]
  where
    title = S.pack $ "Current Bid" ++ show aucTXID
    bidAction = AppAction (SendAuctionAction (BidRequest aucTXID bidFieldValue))

loginForm :: Model -> View Action
loginForm Model {..} =
  div_
    [class_ "login-form"]
    [ header_
        [class_ "header"]
        [ h2_ [] [text "Enter Username"]
        , input_
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
