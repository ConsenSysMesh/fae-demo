{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}


module Types where

import Data.Aeson as A
import Data.Bool
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.IntMap.Lazy as IntMap
import Data.IntMap.Lazy (IntMap)
import qualified Data.JSString as GJS
import qualified Data.Map as M
import Data.Monoid
import Data.Time
import Data.Time.Clock
import GHC.Generics
import Miso
import Miso.String
import qualified Miso.String as S
import Miso.Subscription.WebSocket
import SharedTypes
import Data.Proxy
import Servant.API
import Servant.Utils.Links
   
instance ToMisoString AucTXID where
  toMisoString = toMisoString . show
  fromMisoString = read . show

-- to do remove this and rely on msg
newtype Message =
  Message MisoString
  deriving (Eq, Show, Generic)

instance ToJSON Message

instance FromJSON Message where
  parseJSON =
    withText "Not a valid string" $ \x -> pure (Message (S.toMisoString x))

data TXLogEntry = TXLogEntry
 {   entryTXID :: String
   , entryTimestamp :: UTCTime
   , entryDescription :: String
   , entryUsername :: String
} deriving (Show, Eq)

data Model = Model
  {  uri :: URI
  ,  msg :: Message
  , received :: MisoString
  , auctions :: M.Map AucTXID Auction
  , bidFieldValue :: Int
  , genNumCoinsField :: Int
  , maxBidCountField :: Int
  , auctionStartValField :: Int
  , showBidHistory :: Bool
  , loggedInUsername :: MisoString
  , loggedIn :: Bool
  , selectedAuctionTXID :: Maybe AucTXID
  , accountBalance :: Int
  , txLog :: [TXLogEntry]
  } deriving (Show, Eq, Generic)

data Action
  = AppAction AppAction
  | ServerAction Msg -- incoming ws actions

-- Actions for updating local client state
data AppAction
  = HandleWebSocket (WebSocket Message)
  | SendMessage Message
  | SendServerAction Msg
  | SendCreateAuctionRequest
  | ToggleShowBidHistory
  | UpdateUserNameField MisoString
  | Login
  | Logout
  | UpdateMessage MisoString
  | UpdateBidField (Maybe Int)
  | UpdateGenNumCoinsField (Maybe Int)
  | SelectAuction AucTXID
  | MintCoinsAndBid AucTXID Int
  | UpdateNewMaxBidCountField Int
  | UpdateNewStartingValField Int
  | HandleURI URI
  | ChangeURI URI
  | Noop

-- | Type-level routes
type API   = Login :<|> AuctionHome :<|> Create :<|> Lobby
type AuctionHome = "auctionHome" :> View AppAction
type Create = "create" :> View AppAction
type Login = "login" :> View AppAction
type Lobby = "lobby" :> View AppAction


-- | Type-safe links used in `onClick` event handlers to route the application
goLogin, goCreate, goAuctionHome, goLobby :: AppAction
(goLogin, goCreate, goAuctionHome, goLobby) = (goto api login, goto api create, goto api auctionHome, goto api lobby)
  where
    goto a b = ChangeURI (linkURI (safeLink a b))
    auctionHome = Proxy :: Proxy AuctionHome
    create = Proxy :: Proxy Create
    login = Proxy :: Proxy Login
    lobby = Proxy :: Proxy Lobby
    api   = Proxy :: Proxy API
