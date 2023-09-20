{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module WebPushMock where

import           Control.Lens               hiding ((.=))
import           Data.Aeson
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import           Data.ByteString.Base64.URL
import qualified Data.ByteString.Lazy       as BSL
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as TE
import           Data.Time
import qualified Network.HTTP.Client        as HTTP
import           Network.Wreq
import           System.Process
import           Test.Tasty
import           Test.Tasty.HUnit
import           Web.WebPush

{-
  These are tests to be ran against a mock web-push server from https://github.com/marc1706/web-push-testing
-}

testSendMessage :: TestTree
testSendMessage =
  withResource initWebPushTestingServer terminateProcess $ \_ -> do
    testCaseSteps "Mock subscription" $ \step -> do
      step "Checking status"
      _status <- webPushStatus

      keys <- either fail (pure . readVAPIDKeys) =<< generateVAPIDKeys
      publicKeyBytes <- either fail (pure . BS.pack) $ vapidPublicKeyBytes keys
      let subscriptionOptions = SubscribeOptions True publicKeyBytes

      step "Creating a test subscription to the mock server"
      subscribeResponse <- webPushSubscribe subscriptionOptions
      let subscription = subscribeResponse ^. responseBody

      step "Sending message through the mock server"
      time <- getCurrentTime
      let notification = (mkPushNotification (endpoint subscription) (p256dh subscription) (auth' subscription))
            & pushExpireInSeconds .~ 60 * 60 * 12
            & pushMessage .~ message
            & pushSenderEmail .~ "test@example.com"
          message :: Value
          message = object [
              "title" .= ("Web Push Test" :: Text)
            , "body" .= ("Hello" :: Text)
            , "icon" .= ("" :: Text)
            , "tag" .= Text.pack (show time)
            , "url" .= ("http://localhost:3000" :: Text)
            ]
          encodedMessage = TE.decodeUtf8 $ BSL.toStrict $ encode message
      endpointManager <- HTTP.newManager HTTP.defaultManagerSettings
      either (assertFailure . show) (const (pure ())) =<< sendPushNotification keys endpointManager notification

      step "Getting notifications"
      notificationsResponse <- webPushGetNotificationsFor (clientHash subscription)
      let notifications = notificationsResponse ^. responseBody . to messages
      assertEqual "One notification" 1 (length notifications)
      assertEqual "Notification content" encodedMessage (head notifications)

webPushTestingPort :: Int
webPushTestingPort = 8090

webPushTestingUrl :: String
webPushTestingUrl = "http://localhost:" <> show webPushTestingPort

-- Initializes the web push testing server binary from path
initWebPushTestingServer :: IO ProcessHandle
initWebPushTestingServer = do
  (_stdIn, _stdOut, _stdErr, procHandle) <- createProcess (proc "web-push-testing-server" [show webPushTestingPort])
  pure procHandle

{-
API for the mock server
-}
webPushStatus :: IO (Response BSL.ByteString)
webPushStatus = post (webPushTestingUrl <> "/status") (mempty :: ByteString)

newtype ClientHash = ClientHash {
  unClientHash :: Text
} deriving (Eq, Ord, Show)

instance ToJSON ClientHash where
  toJSON (ClientHash hash) = object ["clientHash" .= hash]

data SubscribeOptions = SubscribeOptions {
  userVisibleOnly :: Bool,
  applicationServerKey :: ByteString
} deriving (Eq, Ord, Show)

instance ToJSON SubscribeOptions where
  toJSON opts = object [
      "userVisibleOnly" .= stringlyBool (userVisibleOnly opts)
    , "applicationServerKey" .= encodeBase64Unpadded (applicationServerKey opts)
    ]
    where
      stringlyBool :: Bool -> Text
      stringlyBool True = "true"
      stringlyBool False = "false"

data SubscriptionResult = SubscriptionResult {
  endpoint :: Text,
  p256dh :: Text,
  auth' :: Text,
  clientHash :: ClientHash
} deriving (Eq, Ord, Show)

instance FromJSON SubscriptionResult where
  parseJSON = withObject "SubscriptionResult" $ \o -> do
    dataObj <- o .: "data"
    SubscriptionResult
      <$> dataObj .: "endpoint"
      <*> (dataObj .: "keys" >>= (.: "p256dh"))
      <*> (dataObj .: "keys" >>= (.: "auth"))
      <*> (ClientHash <$> dataObj .: "clientHash")

webPushSubscribe :: SubscribeOptions -> IO (Response SubscriptionResult)
webPushSubscribe opts = asJSON =<< post (webPushTestingUrl <> "/subscribe") (toJSON opts)

data GetNotifications = GetNotifications {
  messages :: [Text]
} deriving (Eq, Ord, Show)

instance FromJSON GetNotifications where
  parseJSON = withObject "GetNotifications" $ \o -> do
    d <- o .: "data"
    GetNotifications <$> d .: "messages"

webPushGetNotificationsFor :: ClientHash -> IO (Response GetNotifications)
webPushGetNotificationsFor client = asJSON =<< post (webPushTestingUrl <> "/get-notifications") (toJSON client)