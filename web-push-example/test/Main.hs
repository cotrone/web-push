{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.STM
import Test.Tasty
import Control.Monad
import qualified Data.HashMap.Strict as HashMap
import Data.Aeson
import Control.Monad.IO.Class
import qualified Test.Tasty.HUnit as HUnit
import qualified Data.Set as Set
import Web.Api.WebDriver
import System.Process
import Control.Concurrent
import System.IO

import WebPushExample


firefoxCapabilities :: Capabilities
firefoxCapabilities =
  defaultFirefoxCapabilities {
    _firefoxOptions = Just $ FirefoxOptions {
        _firefoxBinary = Nothing
      , _firefoxArgs = Just []
      , _firefoxLog = Nothing
      , _firefoxPrefs = Just $ HashMap.fromList [
          ("permissions.default.desktop-notification", Number 1)
        , ("dom.webnotifications.enabled", Bool True)
        ]
    }
  }

chromeCapabilities :: Capabilities
chromeCapabilities =
  defaultChromeCapabilities {
    _chromeOptions = Just $ defaultChromeOptions {
      _chromePrefs = Just $ HashMap.fromList [
        ("profile.default_content_setting_values.notifications", Number 1) -- 1 is allow, 2 is block, 0 is default 
      ]
    }
  }

chromeConfig :: WebDriverConfig IO
chromeConfig = defaultWebDriverConfig {
    _environment = environment {
      _env = chromeEnv {
        _remotePort = 9515
      }
    } 
  }
  where
    environment = _environment defaultWebDriverConfig
    chromeEnv = _env environment

-- | The webdriver-w3c tasty setup doesn't allow for capabilities to be passed in any way
-- so this is function to make a test case
testWebdriver :: WebDriverConfig IO -> Capabilities -> String -> WebDriverT IO () -> TestTree
testWebdriver config capabilities name action =
  HUnit.testCase name $ do
    (res, summary) <- debug action
    case res of
      Left err -> HUnit.assertFailure $ show err
      Right _ -> do
        when (numFailures summary > 0) $ do
          HUnit.assertFailure $ "Failures: " ++ show (numFailures summary)
  where
    debug = debugWebDriverT config . runIsolated_ capabilities

testChrome :: String -> WebDriverT IO () -> TestTree
testChrome = testWebdriver chromeConfig chromeCapabilities

testFirefox :: String -> WebDriverT IO () -> TestTree
testFirefox = testWebdriver defaultWebDriverConfig firefoxCapabilities

main :: IO ()
main =
  defaultMain $ do
    testGroup "Browsers" [
      withResource (initDriver "chromedriver") killDriver $ \startServer -> testGroup "Chrome" [
        withResource initTestServer killTestServer $ \getTestServer -> localOption (mkTimeout 10000000) $ testChrome "Subscribe" $ do
          _ <- liftIO startServer
          testServer <- liftIO getTestServer
          subscribe testServer
      ]
      , withResource (initDriver "geckodriver") killDriver $ \startServer -> testGroup "Firefox" [
        withResource initTestServer killTestServer $ \getTestServer -> localOption (mkTimeout 10000000) $ testFirefox "Subscribe" $ do
          _ <- liftIO startServer
          testServer <- liftIO getTestServer
          subscribe testServer
      ]
      ]

data TestServer = TestServer {
  testServerThread :: ThreadId
, testServerConfig :: AppConfig
}

withTestServer :: (IO TestServer -> TestTree) -> TestTree
withTestServer = withResource initTestServer killTestServer

initTestServer :: IO TestServer
initTestServer = do
  cfg <- initInMemoryConfig
  thread <- forkIO $ runExampleApp 3000 cfg
  pure $ TestServer thread cfg

killTestServer :: TestServer -> IO ()
killTestServer = killThread . testServerThread

waitForSingleSubscription :: TestServer -> IO ()
waitForSingleSubscription testServer = atomically $ do
  subs <- readTVar $ appConfigSubscriptions $ testServerConfig testServer
  when (Set.null subs) retry
  pure ()

data RunningDriver = RunningDriver {
  runningDriverConfig :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
, runningDriverName :: String
}

initDriver :: String -> IO RunningDriver
initDriver name = do
  p <- createProcess $ proc name []
  pure $ RunningDriver p name

killDriver :: RunningDriver -> IO ()
killDriver driver = do
  putStrLn $ "Terminating driver " <> runningDriverName driver
  cleanupProcess $ runningDriverConfig driver
  putStrLn $ "Terminated driver " <> runningDriverName driver

subscribe :: TestServer -> WebDriverT IO ()
subscribe testServer = do
  liftIO $ putStrLn "Navigating to page"
  navigateTo "http://localhost:3000"
  liftIO $ putStrLn "Finding subscribe button"
  subscribeButton <- findElement XPathSelector "//button"
  liftIO $ print subscribeButton
  liftIO $ putStrLn "Clicking subscribe button"
  elementClick subscribeButton
  liftIO $ do
    putStrLn "Waiting for subscription"
    waitForSingleSubscription testServer
    putStrLn "Got subscription"
    appSendPushNotification (testServerConfig testServer) "test"
  liftIO $ putStrLn "Waiting for notification"
  res <- executeScript "window.alert" ["hello"]
  liftIO $ print res
  return ()