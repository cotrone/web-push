{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
module WebPushExample where


import           Control.Concurrent.STM.TVar
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.Trans.Reader
import qualified Data.Aeson                  as JS
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BSL
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE
import           Data.Time
import           Data.Word
import qualified Network.HTTP.Client         as HTTP
import qualified Network.HTTP.Conduit        as HTTP
import           Network.HTTP.Media          ((//), (/:))
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API
import           Servant.Server.StaticFiles
import           System.Directory
import qualified Text.Mustache               as Mustache
import           Web.FormUrlEncoded
import qualified Web.WebPush                 as WP
import           Web.WebPush                 (sendPushNotification)

import           Templates

-- | Form for sending a push notification, just the text to send
newtype PushNotificationForm = PushNotificationForm { pushNotificationText :: Text }

instance FromForm PushNotificationForm where
  fromForm form = PushNotificationForm <$> parseUnique "text" form

-- | Subscription to push notifications from the browser
data Subscription = Subscription {
  subEndpoint :: Text
, subAuth     :: Text
, subP256dh   :: Text
} deriving (Eq, Ord, Show)

instance JS.FromJSON Subscription where
  parseJSON = JS.withObject "Subscription" $ \obj -> Subscription
    <$> obj JS..: "endpoint"
    <*> obj JS..: "auth"
    <*> obj JS..: "p256dh"

instance JS.ToJSON Subscription where
  toJSON (Subscription endpoint auth p256dh) = JS.object [
      "endpoint" JS..= endpoint
    , "auth" JS..= auth
    , "p256dh" JS..= p256dh
    ]

instance FromForm Subscription where
  fromForm form =
    Subscription
      <$> parseUnique "endpoint" form
      <*> parseUnique "auth" form
      <*> parseUnique "p256dh" form

-- Server API that serves the the static files, subscribes user agents to push notifications, and sends push notifications
type API =
       "send" :> ReqBody '[FormUrlEncoded] PushNotificationForm :> Post '[JSON] () -- ^ Send a push notification to all subscribers
  :<|> "subscribe" :> ReqBody '[FormUrlEncoded] Subscription :> Post '[JSON] () -- ^ Subscribe to notifications
  :<|> Raw -- ^ Serves the static files (index.html, index.js, and service-worker.js) for the browser

-- | Configuration for the web server
data AppConfig = AppConfig
  { appConfigVAPIDKeys            :: WP.VAPIDKeys -- ^ Public and private keys for web-push
  , appConfigVAPIDKeyBytes        :: [Word8] -- ^ Public key for web-push as bytes, passed into the index.js.mustache template
  , appConfigManager              :: HTTP.Manager -- ^ HTTP manager for sending push notifications
  , appConfigSubscriptions        :: TVar (Set Subscription) -- ^ Active subscriptions to push notifications
  , appConfigWriteSubscriptions   :: IO () -- ^ Write subscriptions to a file
  }

-- | Handler for the API, reads the config from the environment
type PushHandler = ReaderT AppConfig Handler

server :: [Word8] -> ServerT API PushHandler
server vapidPublicKey =
       postSendPushNotification
  :<|> postAddSubscriber
  :<|> serveDirectoryEmbedded staticFiles
  where
    -- The static files to serve to the browser
    -- `index.js` has the VAPID public key embedded in it
    staticFiles = [
          ("service-worker.js", serviceWorkerJS )
        , ("index.html", indexHtml)
        , ("index.js", TE.encodeUtf8 $ Mustache.substituteValue indexJsTemplate $ Mustache.object [ "serverKey" Mustache.~> vapidPublicKey ])
      ]

-- | Add a subscriber to the list of subscribers
postAddSubscriber :: Subscription -> PushHandler ()
postAddSubscriber sub = do
  writeSubscriptions <- asks appConfigWriteSubscriptions
  subscriptionRef <- asks appConfigSubscriptions
  liftIO $ do
    putStrLn "Adding subscriber"
    atomically $ modifyTVar' subscriptionRef (Set.insert sub)
    writeSubscriptions

-- | Filepath subscriptions are persisted to
subscriptionsFp :: FilePath
subscriptionsFp = "subscriptions.json"

-- | Write all subscriptions to a file
writeSubscriptions :: TVar (Set Subscription) -> IO ()
writeSubscriptions ref =  do
  subs <- atomically $ readTVar ref
  JS.encodeFile subscriptionsFp subs

-- | Initialize subscriptions from a file if they exist
initPersistentSubscriptions :: IO (TVar (Set Subscription))
initPersistentSubscriptions = do
  e <- doesFileExist subscriptionsFp
  if e
    then do
      subs <- JS.decodeFileStrict subscriptionsFp
      case subs of
        Nothing   -> fail "Unable to read subscriptions"
        Just subs -> newTVarIO subs
    else newTVarIO mempty

-- | Send a push notification to all subscribers
postSendPushNotification :: PushNotificationForm -> PushHandler ()
postSendPushNotification (PushNotificationForm text) = do
  cfg <- ask
  liftIO $ appSendPushNotification cfg text

-- | Send a push notification to all subscribers 
appSendPushNotification :: AppConfig -- ^ Configuration for the application
                        -> Text -- ^ Text to send in the push notification
                        -> IO ()
appSendPushNotification cfg text = do
  -- Get the current time to use as the tag for the notification
  time <- getCurrentTime
  -- Get all the subscriptions
  subscribtions <- atomically $ readTVar subs
  let
    -- This is the message sent to browsers
    message = JS.object [
          "title" JS..= ("Web Push Test" :: Text)
        , "body" JS..= text
        , "icon" JS..= ("" :: Text)
        , "tag" JS..= T.pack (show time)
        , "url" JS..= ("http://localhost:3000" :: Text)
        ]
    pushDetails (Subscription endpoint auth p256dh) =
      (WP.mkPushNotification endpoint p256dh auth) -- The subscription details
        & WP.pushExpireInSeconds .~ 60 * 60 * 12 -- 12 hours
        & WP.pushMessage .~ message -- The message we created above
        & WP.pushSenderEmail .~ "test@example.com" -- The email address of the sender

  liftIO $ putStrLn $ "Sending notification to " <> show (Prelude.length subscribtions) <> " subscribers containing message: " <> show text
  -- Loop over the subscriptions sending each one a notification
  forM_ subscribtions $ \sub -> do
    liftIO $ putStrLn $ "Sending notification to: " <> show sub
    notificationResult <- WP.sendPushNotification keys manager $ pushDetails sub
    liftIO $ putStrLn $ "Notification result: " <> show notificationResult
  where
    keys = appConfigVAPIDKeys cfg
    manager = appConfigManager cfg
    subs = appConfigSubscriptions cfg

runExampleApp :: Int -- ^ Port to run the application on
              -> AppConfig -- ^ Configuration for the application
              -> IO ()
runExampleApp port cfg = run port (serveWithContextT (Proxy :: Proxy API) EmptyContext (flip runReaderT cfg) (server (appConfigVAPIDKeyBytes cfg)))

initInMemoryConfig :: IO AppConfig
initInMemoryConfig = do
  vapidKeys <- either fail (pure . WP.readVAPIDKeys) =<< WP.generateVAPIDKeys
  subscriptions <- newTVarIO mempty
  keyBytes <- either fail pure $ WP.vapidPublicKeyBytes vapidKeys
  manager <- HTTP.newManager HTTP.tlsManagerSettings
  pure $ AppConfig vapidKeys keyBytes manager subscriptions (pure ())

initPersistentConfig :: IO AppConfig
initPersistentConfig = do
  vapidKeys <- initPersistentKeys
  subscriptions <- initPersistentSubscriptions
  keyBytes <- either fail pure $ WP.vapidPublicKeyBytes vapidKeys
  manager <- HTTP.newManager HTTP.tlsManagerSettings
  pure $ AppConfig vapidKeys keyBytes manager subscriptions (writeSubscriptions subscriptions)
  where
    initPersistentKeys :: IO WP.VAPIDKeys
    initPersistentKeys = do
      keysExist <- doesFileExist fp
      if keysExist
        then (maybe (fail "Unable to read keys") (pure . WP.readVAPIDKeys . from)) =<< JS.decodeFileStrict fp
        else do
          putStrLn "Generating keys"
          keys <- either fail pure =<< WP.generateVAPIDKeys
          JS.encodeFile fp $ to keys
          pure $ WP.readVAPIDKeys keys
      where
        fp = "vapid-keys.json"
        to (WP.VAPIDKeysMinDetails n x y) = (n , x , y)
        from (n , x , y) = WP.VAPIDKeysMinDetails n x y
    -- Initialize subscriptions from a file if they exist
    initPersistentSubscriptions :: IO (TVar (Set Subscription))
    initPersistentSubscriptions = do
      e <- doesFileExist subscriptionsFp
      if e
        then do
          subs <- JS.decodeFileStrict subscriptionsFp
          case subs of
            Nothing   -> fail "Unable to read subscriptions"
            Just subs -> newTVarIO subs
        else newTVarIO mempty
    -- Write all subscriptions to a file
    writeSubscriptions :: TVar (Set Subscription) -> IO ()
    writeSubscriptions ref =  do
      subs <- atomically $ readTVar ref
      JS.encodeFile subscriptionsFp subs