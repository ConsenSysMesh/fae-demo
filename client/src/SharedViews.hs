{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module SharedViews where

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

headerView :: Model -> View Action
headerView m = div_ [class_ "header-container"] [titleView, signedInView m ]

-- main title
titleView = h2_ [class_ "title-container header-item"] [text "Fae Auction"]

-- username and user icon
signedInView Model{..} = div_ [class_ "signedin-container"] [
    div_ [class_ "header-item username-container"] [h3_ [class_ "username"] [text username]]
    , 
    div_ [class_ "header-item signedin-logo-container"] [img_ [class_ "signedin-logo", src_ $ S.pack "https://openclipart.org/download/247319/abstract-user-flat-3.svg"]]
    ]

description = S.pack "Lot #685 London: Printed for W. Strahan; and T. Cadell, 1776. First edition of the Adam Smith’s magnum opus and cornerstone of economic thought."

bookImg :: View Action
bookImg = img_ [class_ "book-view", src_ $ S.pack "https://www.baumanrarebooks.com/BookImages/86406.jpg", width_ "250px"]
