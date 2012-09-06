{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, KindSignatures, GADTs #-}

-- Example of using Kansas Comet

module Main where

import Data.Aeson as A hiding ((.=))
import Data.Aeson.Types as AP hiding ((.=))
import qualified Web.Scotty as Scotty
import Web.Scotty (scotty, get, file, literal, middleware)
import Web.KansasComet as KC
import Data.Default
import Data.Map (Map)
import Control.Monad
--import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Data.Monoid
import Data.List as L
import Control.Monad.IO.Class
import Network.Wai.Middleware.Static
-- import Network.Wai      -- TMP for debug

import qualified Data.Text.Lazy as LT
import qualified Data.Text      as T

main = scotty 3000 $ do
    kcomet <- liftIO kCometPlugin

    let pol = only [ ("","index.html")
                   , ("js/kansas-comet.js",kcomet)
                   ]
              <|> ((hasPrefix "css/" <|> hasPrefix "js/") >-> addBase ".")

    middleware $ staticPolicy pol

    connect opts web_app

opts :: KC.Options
opts = def { prefix = "/example", verbose = 0 }

-- This is run each time the page is first accessed
web_app :: Document -> IO ()
web_app doc = do
    registerEvents doc "body" (slide <> click)
    forkIO $ control doc 0
    return ()

control :: Document -> Int -> IO ()
control doc model = do
    Just res <- waitForEvent doc "body" (slide <> click)
    case res of
        Slide _ n                      -> view doc n
        Click "up"    _ _ | model < 25 -> view doc (model + 1)
        Click "down"  _ _ | model > 0  -> view doc (model - 1)
        Click "reset" _ _              -> view doc 0
        _ -> control doc model

view :: Document -> Int -> IO ()
view doc n = do
    send doc $ concat
                [ "$('#slider').slider('value'," ++ show n ++ ");"
                , "$('#fib-out').html('fib " ++ show n ++ " = ...')"
                ]
    -- sent a 2nd packet, because it will take time to compute fib
    send doc ("$('#fib-out').text('fib " ++ show n ++ " = " ++ show (fib n) ++ "')")

    control doc n

fib n = if n < 2 then 1 else fib (n-1) + fib (n-2)

data Event = Slide String Int
           | Click String Int Int
    deriving (Show)

slide = event "slide" Slide
            <&> "id"      .= "$(widget).attr('id')"
            <&> "count"   .= "aux.value"

click = event "click" Click
            <&> "id"      .= "$(widget).attr('id')"
            <&> "pageX"   .=  "event.pageX"
            <&> "pageY"   .=  "event.pageY"

events = slide <> click

