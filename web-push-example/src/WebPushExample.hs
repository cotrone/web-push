{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
module WebPushExample where


import           Control.Monad.STM
import           Control.Concurrent.STM.TVar
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.RWS    (put)
import qualified Data.Aeson                 as JS
import qualified Data.ByteString.Lazy       as BSL
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Data.Time
import           Data.Word
import qualified Network.HTTP.Client        as HTTP
import qualified Network.HTTP.Conduit       as HTTP
import           Network.HTTP.Media         ((//), (/:))
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API
import           System.Directory
import qualified Text.Mustache              as Mustache
import           Web.FormUrlEncoded
import qualified Web.WebPush                as WP
import           Web.WebPush                (sendPushNotification)

import           Templates

-- | HTML content type, this isn't in servant
data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML Text where
  mimeRender _ = BSL.fromStrict . TE.encodeUtf8

-- | JS content type, this isn't in servant
data JS

instance Accept JS where
  contentType _ = "text" // "javascript" /: ("charset", "utf-8")

instance MimeRender JS Text where
  mimeRender _ = BSL.fromStrict . TE.encodeUtf8

-- Server API that serves the the static files, subscribes user agents to push notifications, and sends push notifications
type API =
       "service-worker.js" :> Get '[JS] Text -- ^ Service worker script for hanlding push notifications
  :<|> "index.js" :> Get '[JS] Text -- ^ Index script for registering service worker
  :<|> "send" :> ReqBody '[FormUrlEncoded] PushNotificationForm :> Post '[JSON] () -- ^ Send a push notification to all subscribers
  :<|> "subscribe" :> ReqBody '[FormUrlEncoded] Subscription :> Post '[JSON] () -- ^ Subscribe to notifications
  :<|> Get '[HTML] Text -- ^ Index page

-- | Form for sending a push notification, just the text to send
newtype PushNotificationForm = PushNotificationForm { pushNotificationText :: Text }

instance FromForm PushNotificationForm where
  fromForm form = PushNotificationForm <$> parseUnique "text" form


-- | Handler for the API, reads the config from the environment
type PushHandler = ReaderT AppConfig Handler

server :: ServerT API PushHandler
server =
  serveServiceWorker
  :<|> serveIndexJS
  :<|> postSendPushNotification
  :<|> postAddSubscriber
  :<|> serveIndex

-- | Serves the service worker script from the template
serveServiceWorker :: PushHandler Text
serveServiceWorker = do
  vapidKeys <- asks appConfigVAPIDKeyBytes
  pure $ Mustache.substituteValue serviceWorkerTemplate $ Mustache.object [ "serverKey" Mustache.~> vapidKeys ]

-- | Serves the index page from the template
serveIndex :: PushHandler Text
serveIndex = do
  pure $ Mustache.substituteValue indexTemplate $ Mustache.object [ ]

-- | Serves the index js page from the template
serveIndexJS :: PushHandler Text
serveIndexJS = do
  vapidKeys <- asks appConfigVAPIDKeyBytes
  pure $ Mustache.substituteValue indexJsTemplate $ Mustache.object [ "applicationServerKey" Mustache.~> vapidKeys ]

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

appSendPushNotification :: AppConfig -> Text -> IO ()
appSendPushNotification cfg text = do
  time <- getCurrentTime
  subscribtions <- atomically $ readTVar subs
  let message = JS.object [
          "title" JS..= ("Web Push Test" :: Text)
        , "body" JS..= text
        , "icon" JS..= ("" :: Text)
        , "tag" JS..= T.pack (show time)
        , "url" JS..= ("http://localhost:3000" :: Text)
        ]
      pushDetails (Subscription endpoint auth p256dh) = (WP.mkPushNotification endpoint p256dh auth)
                      & WP.pushExpireInSeconds .~ 60 * 60 * 12
                      & WP.pushMessage .~ message
                      & WP.pushSenderEmail .~ "test@example.com"
      sendToSubscription ds = WP.sendPushNotification keys manager ds

  liftIO $ putStrLn $ "Sending notification to " <> show (Prelude.length subscribtions) <> " subscribers containing message: " <> show text
  forM_ subscribtions $ \sub -> do
    liftIO $ putStrLn $ "Sending notification to: " <> show sub
    notificationResult <- sendToSubscription $ pushDetails sub
    liftIO $ putStrLn $ "Notification result: " <> show notificationResult
  where
    keys = appConfigVAPIDKeys cfg
    manager = appConfigManager cfg
    subs = appConfigSubscriptions cfg

data AppConfig = AppConfig
  { appConfigVAPIDKeys            :: WP.VAPIDKeys
  , appConfigVAPIDKeyBytes        :: [Word8]
  , appConfigManager              :: HTTP.Manager
  , appConfigSubscriptions        :: TVar (Set Subscription)
  , appConfigWriteSubscriptions   :: IO ()
  }

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



runExampleApp :: Int -- ^ Port to run the application on
              -> AppConfig -- ^ Configuration for the application
              -> IO ()
runExampleApp port cfg = run port (serveWithContextT (Proxy :: Proxy API) EmptyContext (flip runReaderT cfg) server)

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