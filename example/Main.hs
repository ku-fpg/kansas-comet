{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, KindSignatures, GADTs #-}

-- Example of using Kansas Comet

module Main (main) where

import qualified Control.Applicative as A
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad

import           Data.Aeson as A hiding ((.=))
import           Data.Aeson.Types as AP hiding ((.=))
import           Data.Default.Class
import           Data.Semigroup
import qualified Data.Text      as T

-- import           Network.Wai      -- TMP for debug
import           Network.Wai.Middleware.Static

import           Web.Scotty.Comet as KC
import           Web.Scotty (scotty, middleware)

main :: IO ()
main = do
    kcomet <- kCometPlugin

    let pol = only [ ("","index.html")
                   , ("js/kansas-comet.js",kcomet)
                   ]
              <|> ((hasPrefix "css/" <|> hasPrefix "js/") >-> addBase ".")

    connectApp <- connect opts web_app

    scotty 3000 $ do
        middleware $ staticPolicy pol
        connectApp

opts :: KC.Options
opts = def { prefix = "/example", verbose = 3 }

-- This is run each time the page is first accessed
web_app :: Document -> IO ()
web_app doc = do
    send doc $ T.unlines
        [ "$('body').on('slide', '.slide', function (event,aux) {"
        , "$.kc.event({eventname: 'slide', count: aux.value });"
        , "});"
        ]
    send doc $ T.unlines
        [ "$('body').on('click', '.click', function (event,aux) {"
        , "$.kc.event({eventname: 'click', id: $(this).attr('id'), pageX: event.pageX, pageY: event.pageY });"
        , "});"
        ]
    void . forkIO $ control doc 0
    return ()

control :: Document -> Int -> IO ()
control doc model = do
    res <- atomically $ readTChan (eventQueue doc)
    case parse parseEvent res of
           Success evt -> case evt of
                   Slide n                        -> view doc n
                   Click "up"    _ _ | model < 25 -> view doc (model + 1)
                   Click "down"  _ _ | model > 0  -> view doc (model - 1)
                   Click "reset" _ _              -> view doc 0
                   _ -> control doc model
           _ -> control doc model

view :: Document -> Int -> IO ()
view doc n = do
    send doc $ T.unlines
                [ "$('#slider').slider('value'," <> T.pack(show n) <> ");"
                , "$('#fib-out').html('fib " <> T.pack(show n) <> " = ...')"
                ]
    -- sent a 2nd packet, because it will take time to compute fib
    send doc ("$('#fib-out').text('fib " <> T.pack(show n) <> " = " <> T.pack(show (fib n)) <> "')")

    control doc n

fib :: Int -> Integer
fib n = if n < 2 then 1 else fib (n-1) + fib (n-2)

parseEvent :: Value -> Parser Event
parseEvent (Object v) = (do
                e :: String <- v .: "eventname"
                n <- v .: "count"
                if e == "slide" then return $ Slide n
                                else mzero) A.<|>
                                (do
                e :: String <- v .: "eventname"
                tag :: String <- v .: "id"
                x :: Int <- v .: "pageX"
                y :: Int <- v .: "pageY"
                if e == "click" then return $ Click tag x y
                                else mzero)
          -- A non-Object value is of the wrong type, so fail.
parseEvent _          = mzero

data Event = Slide Int
           | Click String Int Int
    deriving (Show)
