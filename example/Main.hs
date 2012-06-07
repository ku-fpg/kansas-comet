{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, KindSignatures, GADTs #-}

-- Example of using Kansas Comet

module Main where

import Data.Aeson as A
import Data.Aeson.Types as AP
import qualified Web.Scotty as Scotty
import Web.Scotty (scottyOpts, get, file, literal)
import Web.KansasComet as KC
import Data.Default
import Data.Map (Map)
import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Data.Monoid
import Control.Monad.IO.Class

import qualified Data.Text.Lazy as LT
import qualified Data.Text      as T

main = do
        -- build the scotty dispatch app
        scottyOpts (def { Scotty.verbose = 0 })  $ do
                -- provide some static pages, include jquery
                -- This is scotty code
                get "/" $ file $ "index.html"


                sequence_ [ get (literal ("/" ++ nm)) $ file $  nm
                          | nm <- ["js/jquery.js","js/jquery-json.js","js/jquery-ui.js"] ++ [
                                "css/ui-lightness/jquery-ui.css",
                                "css/ui-lightness/images/ui-bg_diagonals-thick_18_b81900_40x40.png",
                                "css/ui-lightness/images/ui-bg_diagonals-thick_20_666666_40x40.png",
                                "css/ui-lightness/images/ui-bg_flat_10_000000_40x100.png",
                                "css/ui-lightness/images/ui-bg_glass_100_f6f6f6_1x400.png",
                                "css/ui-lightness/images/ui-bg_glass_100_fdf5ce_1x400.png",
                                "css/ui-lightness/images/ui-bg_glass_65_ffffff_1x400.png",
                                "css/ui-lightness/images/ui-bg_gloss-wave_35_f6a828_500x100.png",
                                "css/ui-lightness/images/ui-bg_highlight-soft_100_eeeeee_1x100.png",
                                "css/ui-lightness/images/ui-bg_highlight-soft_75_ffe45c_1x100.png",
                                "css/ui-lightness/images/ui-icons_222222_256x240.png",
                                "css/ui-lightness/images/ui-icons_228ef1_256x240.png",
                                "css/ui-lightness/images/ui-icons_ef8c08_256x240.png",
                                "css/ui-lightness/images/ui-icons_ffd27a_256x240.png",
                                "css/ui-lightness/images/ui-icons_ffffff_256x240.png"]

                          ]
                kcomet <- liftIO kCometPlugin
                get "/js/kansas-comet.js" $ file $ kcomet
                -- connect /example to the following web_app
                connect opts web_app

opts :: KC.Options
opts = def { prefix = "/example", verbose = 0 }

-- This is run each time the page is first accessed
web_app :: Document -> IO ()
web_app doc = do
        print "web_app"

        registerEvents doc (slide <> click)

        let control model = do
                Just res <- waitForEvent doc (slide <> click)
                case res of
                  Slide _ n                      -> view n
                  Click "up"    _ _ | model < 25 -> view (model + 1)
                  Click "down"  _ _ | model > 0  -> view (model - 1)
                  Click "reset" _ _              -> view 0
                  _ -> control model

            view model = do
                let n = model
                send doc ("$('#slider').slider('value'," ++ show n ++ ");")
                send doc ("$('#fib-out').html('fib " ++ show n ++ " = " ++ "&#171;&#8226;&#187;')")
                send doc ("$('#fib-out').text('fib " ++ show n ++ " = " ++ show (fib n) ++ "')")

                control model


        forkIO $ control 0

        return ()

fib n = if n < 2 then 1 else fib (n-1) + fib (n-2)

data Event = Slide String Int
           | Click String Int Int
    deriving (Show)

events' :: [(String,[(String,String)])]
events' = [( "click", [ ("id",           "$(widget).attr('id')")
                     , ("pageX",        "event.pageX")
                     , ("pageY",        "event.pageY")
                     ])
         ,( "slide", [ ("id",           "$(widget).attr('id')")
                     , ("count",        "aux.value")
                     ])
         ]


slide = event "slide" Slide
            <&> "id"      := "$(widget).attr('id')"
            <&> "count"   := "aux.value"

click = event "click" Click
                    <&> "id"      := "$(widget).attr('id')"
                    <&> "pageX"   :=  "event.pageX"
                    <&> "pageY"   :=  "event.pageY"

instance Eventable Event where
       events = [ slide, click ]


--data Match :: * where
--   Match :: Schema a -> (a -> k) -> Match k

--waitForEvent :: [Match e r] -> IO r



