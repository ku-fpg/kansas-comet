{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-- Example of using Kansas Comet

module Main where

import Data.Aeson as A
import Data.Aeson.Types as AP
import Web.Scotty
import Web.KansasComet as KC
import Data.Default
import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class

import qualified Data.Text.Lazy as LT
import qualified Data.Text      as T

main = do
        -- build the scotty dispatch app
        scotty 3000 $ do
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
opts = def { prefix = "/example", verbose = 2 }

-- This is run each time the page is first accessed
web_app :: Document -> IO ()
web_app doc = do
        print "web_app"

        register doc "click" $ concat
                [ " return { pageX : event.pageX"
                , "        , pageY : event.pageY"
                , "        , id    : $(widget).attr('id')"
                , "        };"
                ]

        print "BLA"

        register doc "slide" $ concat
                [ " return ({ id    : $(widget).attr('id')"
                , "         , count  : aux.value"
                , "         });"
                ]


        forkIO $ forever $ do
                Just res <- waitFor doc ["slide","click"]
                case res of
                  Slide _ n -> do
                         send doc ("$('#fib-out').html('fib " ++ show n ++ " = " ++ "&#171;&#8226;&#187;')")
                         send doc ("$('#fib-out').text('fib " ++ show n ++ " = " ++ show (fib n) ++ "')")
                  Click _ x y -> do
                          print ("#########################",x,y)
{-
                print (res :: Value)
                let Success (Slide _ n) :: Result Event = parse parseJSON res
                print n
                --                res <- query doc (Text.pack "return { wrapped : $('#fib-in').attr('value') };")

{-
                let Success (Wrapped a) :: Result (Wrapped String) = parse parseJSON res
                print a
                case reads a of
                  [(v :: Int,"")] -> do
                        send doc (Text.pack $ "$('#fib-out').html('&#171;&#8226;&#187;')")
                        send doc (Text.pack $ "$('#fib-out').text('" ++ show (fib v) ++ "')")
                  _ ->  send doc (Text.pack $ "$('#fib-out').text('...')")
-}

--                let Success b :: Result String = parse parseJSON a
--                print b
--                print res
-}
        return ()

fib n = if n < 2 then 1 else fib (n-1) + fib (n-2)

data Event = Slide String Int
           | Click String Int Int


instance FromJSON Event where
   parseJSON (Object v) =
         (Slide <$> (v .: "id") <*> (v .: "count"))
     <|> (Click <$> (v .: "id") <*> (v .: "pageX") <*> (v .: "pageY"))
   parseJSON _          = mzero

{- How to get a 'wrapper result'

res <- query doc (Text.pack "return { wrapped : $('#fib-in').attr('value') };")

data Wrapped a = Wrapped a
        deriving Show

instance FromJSON a => FromJSON (Wrapped a) where
   parseJSON (Object v) = Wrapped    <$>
                          (v .: "wrapped")
   parseJSON _          = mzero

-}