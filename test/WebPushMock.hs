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
import           Network.URI

{-
  These are tests to be ran against a mock web-push server from https://github.com/marc1706/web-push-testing
-}

testSendMessage :: TestTree
testSendMessage =
  localOption (mkTimeout (10 ^ (7 :: Integer))) $ withResource initWebPushTestingServer terminateProcess $ \_ -> do
    testCaseSteps "Mock subscription" $ \step -> do
      step "Waiting for status to be accessible"
      _ <- webPushStatus

      keys <- either fail pure =<< generateVAPIDKeys
      publicKeyBytes <- either fail pure $ vapidPublicKeyBytes keys
      let subscriptionOptions = SubscribeOptions True $ BS.pack publicKeyBytes

      step "Creating a test subscription to the mock server"
      subscribeResponse <- webPushSubscribe subscriptionOptions
      let subscription = subscribeResponse ^. responseBody

      step "Sending message through the mock server"
      time <- getCurrentTime
      let 
          notification = PushNotification (60 * 60 * 12) message
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
      either (assertFailure . show) (const (pure ())) =<< sendPushNotification endpointManager (VapidConfig "mailto:test@example.com" keys) notification (subscriptionResult subscription)

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
  (_stdIn, Just stdOut, _stdErr, procHandle) <- createProcess mockServer
  _ <- BS.hGetLine stdOut -- Get a single line to wait for the server to be ready
  pure procHandle
  where
    mockServer = (proc "web-push-testing-server" [show webPushTestingPort]) {
        std_out = CreatePipe
      }
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
  subscriptionResult :: Subscription
, clientHash :: ClientHash
} deriving (Eq, Ord, Show)

instance FromJSON SubscriptionResult where
  parseJSON = withObject "SubscriptionResult" $ \o -> do
    dataObj <- o .: "data"
    endpointStr <- dataObj .: "endpoint"
    endpoint <- maybe (fail $ "Unable to parse endpoint: " <> endpointStr) pure $ parseURI endpointStr
    subscription <- Subscription endpoint
                      <$> (dataObj .: "keys" >>= (.: "p256dh"))
                      <*> (dataObj .: "keys" >>= (.: "auth"))
    SubscriptionResult subscription <$> (ClientHash <$> dataObj .: "clientHash")

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