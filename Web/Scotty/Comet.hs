{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, KindSignatures, GADTs #-}
module Web.Scotty.Comet
    ( connect
    , kCometPlugin
    , send
    , Document
    , Options(..)
    , getReply
    , eventQueue
    , debugDocument
    , debugReplyDocument
    , defaultOptions
    , socketApplication
    ) where

import Web.Scotty (ScottyM, text, post, capture, param, setHeader, get, ActionM, jsonData)
import Data.Aeson hiding ((.=))
import Control.Monad
import Control.Concurrent.STM as STM
import Control.Concurrent.MVar as STM
import Control.Monad.IO.Class
import Paths_kansas_comet
import qualified Data.Map as Map
import Control.Concurrent
import Data.Default
import Data.Maybe ( fromJust )
import qualified Data.HashMap.Strict as HashMap
import System.Exit
import qualified Data.Text.Lazy as LT
import qualified Data.Text      as T
import Data.Time.Calendar
import Data.Time.Clock
import Numeric
import qualified Network.WebSockets as WS

-- Opening web-socket connection and handling calling the call-back function
-- Adding websocket connection to Docuement object

socketApplication :: (Document -> IO ()) -> WS.PendingConnection -> IO ()
socketApplication callback pending = do
    conn <- WS.acceptRequest pending
    evnt::T.Text <- WS.receiveData conn
    picture <- atomically $ newEmptyTMVar
    callbacks <- atomically $ newTVar $ Map.empty
    queue <- atomically $ newTChan
    let cxt = Document picture callbacks queue 0 (Just conn)
    forkIO $ callback cxt
    receiveEvents conn cxt

-- Continuously listening to incoming socket connection

receiveEvents :: WS.Connection -> Document -> IO()
receiveEvents conn document = forever $ do
          --  WS.sendPing conn (BC.pack "ping")
            evnt <- WS.receiveData conn
            let wrappedVal = fromJust $ decode' evnt
            let val =  let (Object m) = wrappedVal
                       in HashMap.lookup (T.pack "reply") m
            let uq =  let (Object m) = wrappedVal
                       in HashMap.lookup (T.pack "uq") m
            case val of
                Nothing  -> liftIO $ atomically $ do
                                      let pingval =  let (Object m) = wrappedVal
                                                 in HashMap.lookup (T.pack "ping") m
                                      case pingval of
                                          Nothing -> writeTChan (eventQueue document) wrappedVal
                                          Just pingvalue -> return ()

                Just replyval -> liftIO $ atomically $ do
                                      case uq of
                                          Nothing ->   return ()
                                          Just uqVal -> do
                                                        m <- readTVar (replies document)
                                                        case fromJSON uqVal of
                                                          Error msg -> fail msg
                                                          Success a -> writeTVar (replies document) $ Map.insert a replyval m

-- | connect "/foobar" (...) gives a scotty session that:
--
-- >  POST http://.../foobar/                       <- bootstrap the interaction
-- >  GET  http://.../foobar/act/<id#>/<act#>       <- get a specific action
-- >  POST http://.../foobar/reply/<id#>/<reply#>   <- send a reply as a JSON object

connect :: Options             -- ^ URL path prefix for this page
        -> (Document -> IO ()) -- ^ called for access of the page
        -> ScottyM ()
connect opt callback = do
   if not rtsSupportsBoundThreads  -- we need the -threaded flag turned on
   then liftIO $ do putStrLn "Application needs to be re-compiled with -threaded flag"
                    exitFailure
   else return ()


   when (verbose opt >= 1) $ liftIO $ putStrLn $ "kansas-comet connect with prefix=" ++ show (prefix opt)

   -- A unique number generator, or ephemeral generator.
   -- This is the (open) secret between the client and server.
   -- (Why are we using an MVar vs a TMVar? No specific reason here)
   uniqVar <- liftIO $ newMVar 0
   let getUniq :: IO Int
       getUniq = do
              u <- takeMVar uniqVar
              putMVar uniqVar (u + 1)
              return u

   tm ::  UTCTime  <- liftIO $ getCurrentTime

   let server_id
           = Numeric.showHex (toModifiedJulianDay (utctDay tm))
           $ ("-" ++)
           $ Numeric.showHex (floor (utctDayTime tm * 1000) :: Integer)
           $ ""

   contextDB <- liftIO $ atomically $ newTVar $ (Map.empty :: Map.Map Int Document)
   let newContext :: IO Int
       newContext = do
            uq <- getUniq
            picture <- atomically $ newEmptyTMVar
            callbacks <- atomically $ newTVar $ Map.empty
            queue <- atomically $ newTChan
            let cxt = Document picture callbacks queue uq Nothing
            liftIO $ atomically $ do
                    db <- readTVar contextDB
                    -- assumes the getUniq is actually unique
                    writeTVar contextDB $ Map.insert uq cxt db
            -- Here is where we actually spawn the user code
            _ <- forkIO $ callback cxt
            return uq

   -- POST starts things off.
   post (capture $ prefix opt ++ "/") $ do
            uq  <- liftIO $ newContext
            text (LT.pack $ "$.kc.session(" ++ show server_id ++ "," ++ show uq ++ ");")

   -- GET the updates to the documents (should this be an (empty) POST?)

--   liftIO $ print $ prefix opt ++ "/act/:id/:act"
   get (capture $ prefix opt ++ "/act/" ++ server_id ++ "/:id/:act") $ do
            setHeader "Cache-Control" "max-age=0, no-cache, private, no-store, must-revalidate"
            -- do something and return a new list of commands to the client
            num <- param "id"

            when (verbose opt >= 2) $ liftIO $ putStrLn $
                "Kansas Comet: get .../act/" ++ show num
--            liftIO $ print (num :: Int)

            let tryPushAction :: TMVar T.Text -> Int -> ActionM ()
                tryPushAction var n = do
                    -- The PUSH archtecture means that we wait upto 3 seconds if there
                    -- is not javascript to push yet. This stops a busy-waiting
                    -- (or technically restricts it to once every 3 second busy)
                    ping <- liftIO $ registerDelay (3 * 1000 * 1000)
                    res <- liftIO $ atomically $ do
                            b <- readTVar ping
                            if b then return Nothing else do
                                 liftM Just (takeTMVar var)


                    when (verbose opt >= 2) $ liftIO $ putStrLn $
                                "Kansas Comet (sending to " ++ show n ++ "):\n" ++ show res

                    case res of
                     Just js -> do
--                            liftIO $ putStrLn $ show js
                            text $ LT.fromChunks [js]
                     Nothing  ->
                            -- give the browser something to do (approx every 3 seconds)
                            text LT.empty

            db <- liftIO $ atomically $ readTVar contextDB
            case Map.lookup num db of
               Nothing  -> text (LT.pack $ "console.warn('Can not find act #" ++ show num ++ "');")
               Just doc -> tryPushAction (sending doc) num


   post (capture $ prefix opt ++ "/reply/" ++ server_id ++ "/:id/:uq") $ do
           setHeader "Cache-Control" "max-age=0, no-cache, private, no-store, must-revalidate"
           num <- param "id"
           uq :: Int <- param "uq"
           --liftIO $ print (num :: Int, event :: String)

           when (verbose opt >= 2) $ liftIO $ putStrLn $
                "Kansas Comet: post .../reply/" ++ show num ++ "/" ++ show uq

           wrappedVal :: Value <- jsonData
    --       liftIO $ putStrLn $ show wrappedVal
           -- Unwrap the data wrapped, because 'jsonData' only supports
           -- objects or arrays, but not primitive values like numbers
           -- or booleans.
           let val = fromJust $ let (Object m) = wrappedVal
                                in HashMap.lookup (T.pack "data") m
           --liftIO $ print (val :: Value)
           db <- liftIO $ atomically $ readTVar contextDB
           case Map.lookup num db of
               Nothing  -> do
                   text (LT.pack $ "console.warn('Ignore reply for session #" ++ show num ++ "');")
               Just doc -> do
                   liftIO $ do
                         atomically $ do
                           m <- readTVar (replies doc)
                           writeTVar (replies doc) $ Map.insert uq val m
                   text $ LT.pack ""


   post (capture $ prefix opt ++ "/event/" ++ server_id ++ "/:id") $ do
           setHeader "Cache-Control" "max-age=0, no-cache, private, no-store, must-revalidate"
           num <- param "id"

           when (verbose opt >= 2) $ liftIO $ putStrLn $
                "Kansas Comet: post .../event/" ++ show num

           wrappedVal :: Value <- jsonData
           -- Unwrap the data wrapped, because 'jsonData' only supports
           -- objects or arrays, but not primitive values like numbers
           -- or booleans.
           let val = fromJust $ let (Object m) = wrappedVal
                                in HashMap.lookup (T.pack "data") m
           --liftIO $ print (val :: Value)

           db <- liftIO $ atomically $ readTVar contextDB
           case Map.lookup num db of
               Nothing  -> do
                   text (LT.pack $ "console.warn('Ignore reply for session #" ++ show num ++ "');")
               Just doc -> do
                   liftIO $ atomically $ do
                           writeTChan (eventQueue doc) val
                   text $ LT.pack ""

   return ()

-- | 'kCometPlugin' provides the location of the Kansas Comet jQuery plugin.
kCometPlugin :: IO String
kCometPlugin = do
        dataDir <- getDataDir
        return $ dataDir ++ "/static/js/kansas-comet.js"

-- | 'send' sends a javascript fragement to a document.
-- Checks for socket connection if present uses socket connection to send the document.
-- The Text argument will be evaluated before sending (in case there is an error,
-- or some costly evaluation needs done first).
-- 'send' suspends the thread if the last javascript has not been *dispatched*
-- the the browser.
send :: Document -> T.Text -> IO ()
send doc js = do
  case (socketConnection doc) of
    Just connection -> WS.sendTextData connection $ js
    Nothing -> atomically $ putTMVar (sending doc) $! js


-- | wait for a virtual-to-this-document's port numbers' reply.
getReply :: Document -> Int -> IO Value
getReply doc num = do
        atomically $ do
           db <- readTVar (replies doc)
           case Map.lookup num db of
              Nothing -> retry
              Just r -> do
                      writeTVar (replies doc) $ Map.delete num db
                      return r

-- | 'Document' is the Handle into a specific interaction with a web page.
data Document = Document
        { sending          :: TMVar T.Text             -- ^ Code to be sent to the browser
                                                       -- This is a TMVar to stop the generation
                                                       -- getting ahead of the rendering engine
        , replies          :: TVar (Map.Map Int Value) -- ^ This is numbered replies, to ports
        , eventQueue       :: TChan Value              -- ^ Events being sent
        , _secret          :: Int                      -- ^ the (session) number of this document
        , socketConnection :: Maybe (WS.Connection)    -- Socket connection if cleint is using websocket
        }

-- 'Options' for Comet.
data Options = Options
        { prefix  :: String             -- ^ what is the prefix at at start of the URL (for example \"ajax\")
        , verbose :: Int                -- ^ 0 == none (default), 1 == inits, 2 == cmds done, 3 == complete log
        }

instance Default Options where
  def = Options
        { prefix = ""                   -- default to root, this assumes single page, etc.
        , verbose = 0
        }


-- Defaults for 'Options'. Or you can use the defaults package.
defaultOptions :: Options
defaultOptions = def

------------------------------------------------------------------------------------

-- | Generate a @Document@ that prints what it would send to the server.
debugDocument :: IO Document
debugDocument = do
  picture <- atomically $ newEmptyTMVar
  callbacks <- atomically $ newTVar $ Map.empty
  _ <- forkIO $ forever $ do
          res <- atomically $ takeTMVar $ picture
          putStrLn $ "Sending: " ++ show res
  q <- atomically $ newTChan
  return $ Document picture callbacks q 0 Nothing

-- | Fake a specific reply on a virtual @Document@ port.
debugReplyDocument :: Document -> Int -> Value -> IO ()
debugReplyDocument doc uq val = atomically $ do
   m <- readTVar (replies doc)
   writeTVar (replies doc) $ Map.insert uq val m
