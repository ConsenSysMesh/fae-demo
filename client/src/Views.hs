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

import Data.Proxy
import Servant.API

appView :: Model -> View Action
appView m = view
  where
  view =
    either (const the404) id
      $ runRoute (Proxy :: Proxy API) handlers uri m
  handlers = login :<|> home
  home (_ :: Model) = div_ [] [
     homeView m 
    ]
  login (_ :: Model) = div_ [] [
     loginView m
    ]
  the404 = div_ [] [
      text "404 :("
    , button_ [ onClick (AppAction goLogin) ] [ text "Login" ]
    ]

loginView :: Model -> View Action
loginView m@Model {..} = div_ [] [
    loginForm m,
    h1_ [class_ "heading"] [ text "Auctions Powered by Fae"]
  ]

homeView :: Model -> View Action
homeView m@Model {..} =
  div_ [style_ $ M.fromList [("text-align", "center")]] $
  [ div_
      [class_ "auctions-container "]
      auctionViews
  ] ++ [ viewAccBalance accountBalance ]
  where
    auctionViews =
      [mainBtns m] ++
      [viewAuctionsTable m | not $ M.null auctions] ++ [selectedAuctionView m]

mainBtns :: Model -> View Action
mainBtns Model {..} =
  div_ [ class_ "main-btns" ] $
    [button_
      [ 
          onClick (AppAction (SendServerAction (CreateAuctionRequest)))
      ]
      [text "Create New Auction"]
      ] ++ [genCoinNumInput genNumCoinsField]
    
genCoinNumInput :: Int -> View Action
genCoinNumInput coinCount =
  div_
    [class_ "gen-coin-input-container"]
    [ input_
        [ class_ "field-input"
        , type_ "text"
        , autofocus_ True
        , value_ $ S.pack $ show coinCount
        , name_ "numCoinsGen"
        , onInput $
          AppAction . UpdateGenNumCoinsField . readMaybe . S.unpack . S.toMisoString
        ],
         button_ [class_ "get-coin-btn", onClick getCoinsAction] [text "Mint Coins"]
        ]
  where
    title = S.pack $ "Coins"
    getCoinsAction =  AppAction (SendServerAction (RequestCoins coinCount))

bookImg :: View Action
bookImg = img_ [class_ "book-view", src_ $ S.pack "https://www.baumanrarebooks.com/BookImages/86406.jpg", width_ "250px"]
  
selectedAuctionView :: Model -> View Action
selectedAuctionView Model {..} =
  case selectedAuctionTXID of
    Nothing -> ""
    (Just aucTXID) ->
      case M.lookup aucTXID auctions of
        Nothing -> ""
        (Just auction) ->
          auctionView aucTXID auction bidFieldValue (S.fromMisoString username) accountBalance
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

description = S.pack "Lot #685 London: Printed for W. Strahan; and T. Cadell, 1776. First edition of the Adam Smith’s magnum opus and cornerstone of economic thought."

auctionView :: AucTXID -> Auction -> Int -> String -> Int -> View Action
auctionView aucTXID@(AucTXID txid) auction@Auction {..} bidFieldValue username accountBalance =
  div_
    [class_ "auction-view"]
    [ article_ [class_ "card"] $
      [ header_
          [ class_ "place-bid-header"
          , style_ $ M.fromList [(S.pack "text-align", S.pack "center")]
          ]
          [
            h3_ [] [text $ "Auction " <> (S.pack $ show truncatedAucTXID)],
            div_ [class_ "auction-info"] [ h3_ [class_ "book_title"] [
              text $ S.pack "An Inquiry into the Nature and Causes of the Wealth of Nations. Smith, Adam. 1776"], p_ [class_ "book-description"] [text description], bookImg]
          ]
      ] ++
      [ footer_ [] [placeBidView aucTXID auction bidFieldValue username canBid]
      | not $ auctionEnded auction
      ]
    ]
  where
    bidValue = currentBidValue auction
    canBid = accountBalance > bidValue
    truncatedAucTXID = Li.take 5 txid

placeBidView :: AucTXID -> Auction -> Int -> String -> Bool -> View Action
placeBidView aucTXID auction@Auction {..} bidFieldValue username canBid =
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
    , button_ [class_ "bid-field-btn", onClick bidAction, disabled_ $ auctionEnded auction] [text "Place Bid"]
    ]
  where
    title = S.pack $ "Current Bid" ++ show aucTXID
    bidAction = AppAction $ MintCoinsAndBid aucTXID bidFieldValue

viewAccBalance :: Int ->  View Action
viewAccBalance accountBalance =
  div_
    [class_ "account-balance"]
    [ 
      h4_ [] [text $ "Account Balance: " <> (S.pack $ show accountBalance) <> ( bool  " Coins"  " Coin" (accountBalance == 1))]
    ]

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
