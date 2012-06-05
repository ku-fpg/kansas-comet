{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Web.KansasComet where

import Web.Scotty
import Data.Aeson
import Control.Monad
import Control.Concurrent.STM as STM
import Control.Concurrent.MVar as STM
import Control.Monad.IO.Class
import Paths_kansas_comet
import qualified Data.Map as Map
import Control.Concurrent
import Data.Default

import qualified Data.Text.Lazy as LT
import qualified Data.Text      as T

-- | connect "/foobar" (...) gives

-- >  POST http://.../foobar/                       <- bootstrap the interaction
-- >  GET  http://.../foobar/act/<id#>/<act#>       <- get a specific action
-- >  POST http://.../foobar/reply/<id#>/<reply#>   <- send a reply as a JSON object

connect :: Options             -- ^ URL path prefix for this page
        -> (Document -> IO ()) -- ^ called for access of the page
        -> ScottyM ()
connect opt callback = do
   when (verbose opt >= 1) $ liftIO $ putStrLn $ "kansas-comet connect with prefix=" ++ show (prefix opt)

   -- A unique number generator, or ephemeral generator.
   -- This is the (open) secret between the client and server.
   -- (Why are we using an MVar vs a TMVar? No specific reason here)
   uVar <- liftIO $ newMVar 0
   let getUniq :: IO Int
       getUniq = do
              u <- takeMVar uVar
              putMVar uVar (u + 1)
              return u

   contextDB <- liftIO $ atomically $ newTVar $ (Map.empty :: Map.Map Int Document)
   let newContext :: IO Int
       newContext = do
            uq <- getUniq
            picture <- atomically $ newEmptyTMVar
            callbacks <- atomically $ newTVar $ Map.empty
            let cxt = Document picture callbacks uq
            liftIO $ atomically $ do
                    db <- readTVar contextDB
                    -- assumes the getUniq is actually unique
                    writeTVar contextDB $ Map.insert uq cxt db
            -- Here is where we actually spawn the user code
            _ <- forkIO $ callback cxt
            return uq

   -- POST starts things off.
   post (capture $ prefix opt ++ "/") $ do
            liftIO $ print "got root"
            uq  <- liftIO $ newContext
            text (LT.pack $ "$.kc.session(" ++ show uq ++ ");")

   -- GET the updates to the documents (should this be an (empty) POST?)

   liftIO $ print $ prefix opt ++ "/act/:id/:act"
   get (capture $ prefix opt ++ "/act/:id/:act") $ do
            header "Cache-Control" "max-age=0, no-cache, private, no-store, must-revalidate"
            -- do something and return a new list of commands to the client
            num <- param "id"

            when (verbose opt >= 2) $ liftIO $ putStrLn $
                "Kansas Comet: get .../act/" ++ show num
--            liftIO $ print (num :: Int)

            let tryPushAction :: TMVar T.Text -> ActionM ()
                tryPushAction var = do
                    -- The PUSH archtecture means that we wait upto 3 seconds if there
                    -- is not javascript to push yet. This stops a busy-waiting
                    -- (or technically restricts it to once every 3 second busy)
                    ping <- liftIO $ registerDelay (3 * 1000 * 1000)
                    res <- liftIO $ atomically $ do
                            b <- readTVar ping
                            if b then return Nothing else do
                                 liftM Just (takeTMVar var)

                    case res of
                     Just js -> do
                            liftIO $ putStrLn $ show js
                            text $ LT.pack $ T.unpack js
                     Nothing  ->
                            -- give the browser something to do (approx every second)
                            text (LT.pack "")

            db <- liftIO $ atomically $ readTVar contextDB
            case Map.lookup num db of
               Nothing  -> text (LT.pack $ "alert('Can not find act #" ++ show num ++ "');")
               Just doc -> tryPushAction (sending doc)


   post (capture $ prefix opt ++ "/reply/:id/:uq") $ do
           header "Cache-Control" "max-age=0, no-cache, private, no-store, must-revalidate"
           num <- param "id"
           uq :: Int <- param "uq"
--           liftIO $ print (num :: Int, event :: String)
           val :: Value <- jsonData
--           liftIO $ print (val :: Value)
           db <- liftIO $ atomically $ readTVar contextDB
           case Map.lookup num db of
               Nothing  -> do
                   liftIO $ print ("ignoring reply",uq,val)
                   text (LT.pack $ "alert('Ignore reply for session #" ++ show num ++ "');")
               Just doc -> do
                   liftIO $ do
                         atomically $ do
                           m <- readTVar (listening doc)
                           writeTVar (listening doc) $ Map.insert uq val m
                   text $ LT.pack ""

   return ()

-- 'kCometPlugin' provide access to the Kansas Comet jQuery plugin. The argument
-- is the path to the collection that holds the static javascript files.
kCometPlugin :: IO String
kCometPlugin = do
        dataDir <- getDataDir
        return $ dataDir ++ "/static/js/kansas-comet.js"

-- 'send' sends a javascript fragement to a document.
-- The string argument will be evaluated before sending (in case there is an error,
-- or some costly evaluation needs done first).
-- 'send' suspends the thread if the last javascript has not been *dispatched*
-- the the browser.
send :: Document -> String -> IO ()
send doc js = atomically $ putTMVar (sending doc) $! T.pack js

{-
-- | listen sets up a RESTful listener and a new Channel that listens
-- to this listener. It is important to realize that if you call listen twice,
-- you get the *same* channel.
listen :: Document -> EventName -> IO (TChan Value)
listen doc eventName = atomically $ do
        db <- readTVar (listening doc)
        case Map.lookup eventName db of
          Just ch -> return ch
          Nothing -> do
             ch <- newTChan
             writeTVar (listening doc) $ Map.insert eventName ch db
             return ch
-}

-- The Text argument returns an object, which is what part of the event get sent to Haskell.
register :: Document -> EventName -> String -> IO ()
register doc eventName eventBuilder =
        send doc $ concat
                        [ "$.kc.register(" ++ show eventName ++ ",function(widget,event,aux) {"
                        , eventBuilder
                        , "});"
                        ]

waitFor :: Document -> EventName -> IO Value
waitFor doc eventName = do
        let uq = 1023949 :: Int -- later, have this random generated
        send doc $ concat
                [ "$.kc.waitFor(" ++ show eventName ++ ",function(e) { $.kc.reply(" ++ show uq ++ ",e);});" ]
        getReply doc uq

-- internal function, waits for a numbered reply
getReply :: Document -> Int -> IO Value
getReply doc num = do
        atomically $ do
           db <- readTVar (listening doc)
           case Map.lookup num db of
              Nothing -> retry
              Just r -> do
                      writeTVar (listening doc) $ Map.delete num db
                      return r

-- TODO: make thread safe
-- The test ends with a return for the value you want to see.
query :: Document -> String -> IO Value
query doc qText = do
        let uq = 37845 :: Int -- should be uniq
        send doc $ concat
                [ "$.kc.reply(" ++ show uq ++ ",function(){"
                , qText
                , "}());"
                ]
        getReply doc uq


type EventName = String

data Document = Document
        { sending   :: TMVar T.Text             -- ^ Code to be sent to the browser
                                                -- This is a TMVar to stop the generation
                                                -- getting ahead of the rendering engine
        , listening :: TVar (Map.Map Int Value) -- ^ This is numbered replies.
        , secret    :: Int                      -- ^ the number of this document
        }

data Options = Options
        { prefix  :: String
        , verbose :: Int                -- 0 == none, 1 == inits, 2 == cmds done, 3 == complete log
        }

instance Default Options where
  def = Options
        { prefix = ""                   -- default to root, this assumes single page, etc.
        , verbose = 1
        }


