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
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.JSString as GJS
import qualified Data.List as Li
import qualified Data.Map as M
import qualified Data.Map as M
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
      [viewAuctionsTable m | not $ IntMap.null auctions] ++
      [selectedAuctionView m]

selectedAuctionView :: Model -> View Action
selectedAuctionView Model {..} =
  case selectedAuction of
    Nothing -> ""
    (Just auction) ->
      auctionView auction bidFieldValue (S.fromMisoString username)
  where
    selectedAuction = IntMap.lookup selectedAuctionId auctions

onEnter :: Action -> Attribute Action
onEnter action = onKeyDown $ bool (AppAction Noop) action . (== KeyCode 13)

viewAuctions :: IntMap Auction -> View Action
viewAuctions auctions =
  div_ [class_ "auctions"] [ul_ [class_ "auction-list"] auctionItems]
  where
    auctionItems = Prelude.map viewAuction $ IntMap.elems auctions

viewAuction :: Auction -> View Action
viewAuction auction@Auction {..} =
  li_
    [class_ "auction-item", onClick $ AppAction (SelectAuction auctionId)]
    [auctionText]
  where
    auctionText = p_ [] [text $ S.pack $ show auction]

getTableHeader :: [String] -> [View Action]
getTableHeader names =
  Prelude.map (\name -> th_ [] [p_ [] [text $ S.ms name]]) names

iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%H:%M:%S"

--use reader monad for selectedAuctionId
getTableRow :: Int -> Auction -> View Action
getTableRow selectedAuctionId auction@Auction {..} =
  tr_
    [ onClick $ AppAction (SelectAuction auctionId)
    , class_ $ do bool "auction-row" "auction-row-selected" isSelected
    ]
    [ td_ [] [text $ S.ms auctionId]
    , td_ [] [text $ S.ms createdBy]
    , td_ [] [text $ S.ms $ iso8601 createdTimestamp]
    , td_ [] [text $ S.ms numBidToMaxRatio]
    , td_ [] [text $ S.ms $ currentBidValue auction]
    , td_ [] [text $ S.ms $ auctionStatus auction]
    ]
  where
    isSelected = auctionId == selectedAuctionId
    numBidToMaxRatio = (show $ numBids auction) <> "/" <> (show maxNumBids)

getTableRows :: [Auction] -> Int -> [View Action]
getTableRows auctions selectedAuctionId =
  Prelude.map (getTableRow selectedAuctionId) auctions

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
        , tbody_ [] $ getTableRows (IntMap.elems auctions) selectedAuctionId
        ]
    ]

createAuctionView :: Model -> View Action
createAuctionView Model {..} =
  button_
    [ style_ $ M.singleton "font-size" "1.25em"
    , onClick (AppAction (SendAuctionAction (CreateAuctionAction newAuction)))
    ]
    [text "Create New Auction"]
  where
    newAuction =
      Auction
        { auctionId = getNextAuctionKey auctions
        , bids = []
        , createdBy = S.fromMisoString username
        , initialValue = 0
        , maxNumBids = 5
        , createdTimestamp = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)
        }

auctionView :: Auction -> Int -> String -> View Action
auctionView auction@Auction {..} bidFieldValue username =
  div_
    [class_ "auction-view"]
    [ article_ [class_ "card"] $
      [ header_
          [ class_ "header"
          , style_ $ M.fromList [(S.pack "text-align", S.pack "center")]
          ]
          [h3_ [] [text $ "Auction " <> (S.pack $ show auctionId)]]
      ] ++
      [ footer_ [] [placeBidView auction bidFieldValue username]
      | not auctionEnded
      ]
    ]
  where
    auctionEnded = numBids auction == maxNumBids

placeBidView :: Auction -> Int -> String -> View Action
placeBidView auction@Auction {..} bidFieldValue username =
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
    title = S.pack $ "Current Bid" ++ show auctionId
    bid =
      Bid
        { bidValue = bidFieldValue
        , bidder = username
        , bidTimestamp = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)
        }
    bidAction = AppAction (SendAuctionAction (BidAuctionAction auctionId bid))

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
