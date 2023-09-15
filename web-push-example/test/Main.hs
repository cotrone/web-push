{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.HashMap.Strict    as HashMap
import qualified Data.Set               as Set
import qualified Data.Text              as Text
import           System.IO
import           System.Process
import           Test.Tasty
import qualified Test.Tasty.HUnit       as HUnit
import           Web.Api.WebDriver
import           System.Environment
import           System.Posix.Signals (signalProcess, sigINT)

import           WebPushExample


firefoxCapabilities :: Maybe String -> Capabilities
firefoxCapabilities binLocation =
  defaultFirefoxCapabilities {
    _firefoxOptions = Just $ FirefoxOptions {
        _firefoxBinary = binLocation
      , _firefoxArgs = Just ["--headless"]
      , _firefoxLog = Nothing
      , _firefoxPrefs = Just $ HashMap.fromList [
          ("permissions.default.desktop-notification", Number 1)
        , ("dom.webnotifications.enabled", Bool True)
        ]
    }
  }

firefoxConfig :: WebDriverConfig IO
firefoxConfig = defaultWebDriverConfig {
    _environment = defaultWebDriverEnvironment {
      _logOptions = defaultWebDriverLogOptions {
        _logSilent = False
      }
    } 
  }

chromeCapabilities :: Maybe String -> Capabilities
chromeCapabilities binLocation =
  defaultChromeCapabilities {
    _chromeOptions = Just $ defaultChromeOptions {
      _chromeBinary = binLocation
      , _chromeArgs = Just ["--headless=new"]
      , _chromePrefs = Just $ HashMap.fromList [
        ("profile.default_content_setting_values.notifications", Number 1) -- 1 is allow, 2 is block, 0 is default 
      ]
    }
  }

chromeConfig :: WebDriverConfig IO
chromeConfig = defaultWebDriverConfig {
    _environment = defaultWebDriverEnvironment {
      _logOptions = defaultWebDriverLogOptions {
        _logSilent = False
      },
      _env = defaultWDEnv {
        _remotePort = 9515
      }
    } 
  }

-- | The webdriver-w3c tasty setup doesn't allow for capabilities to be passed in any way
-- so this is function to make a test case
testWebdriver :: WebDriverConfig IO -> Capabilities -> String -> WebDriverT IO () -> TestTree
testWebdriver config capabilities name action =
  HUnit.testCase name $ do
    (res1, summary) <- debug $ do
      r <- Right <$> action
      pure r
    case res1 of
      Left err -> HUnit.assertFailure $ show err
      Right res -> case res of
        Left (e :: SomeException) -> HUnit.assertFailure $ show e
        Right _ -> do
          when (numFailures summary > 0) $ do
            HUnit.assertFailure $ "Failures: " ++ show (numFailures summary)
  where
    debug = debugWebDriverT config . runIsolated capabilities

testChrome :: Maybe String -> String -> WebDriverT IO () -> TestTree
testChrome binLocation = testWebdriver chromeConfig (chromeCapabilities binLocation)

testFirefox :: Maybe String -> String -> WebDriverT IO () -> TestTree
testFirefox binLocation = testWebdriver firefoxConfig (firefoxCapabilities binLocation)

main :: IO ()
main = do
  chromeBin <- lookupEnv "CHROME_BINARY"
  firefoxBin <- lookupEnv "FIREFOX_BINARY"
  print (chromeBin, firefoxBin)
  defaultMain $ 
    localOption (mkTimeout 20000000) $ testGroup "Browsers" [
        testGroup "Chrome" [
          withResource initTestServer killTestServer $ \getTestServer -> testChrome chromeBin "Subscribe" $ do
            testServer <- liftIO getTestServer
            subscribe testServer
          ]
          , testGroup "Firefox" [
            withResource initTestServer killTestServer $ \getTestServer -> testFirefox firefoxBin "Subscribe" $ do
              testServer <- liftIO getTestServer
              subscribe testServer
          ]
        ]

data TestServer = TestServer {
  testServerThread :: ThreadId
, testServerConfig :: AppConfig
}

initTestServer :: IO TestServer
initTestServer = do
  cfg <- initInMemoryConfig
  thread <- forkIO $ runExampleApp 3000 cfg
  pure $ TestServer thread cfg

killTestServer :: TestServer -> IO ()
killTestServer server = do
  putStrLn "Killing test server"
  killThread $ testServerThread server

waitForSingleSubscription :: TVar Bool -> TestServer -> IO Bool
waitForSingleSubscription delay testServer = do
  atomically $ do
    subs <- readTVar $ appConfigSubscriptions $ testServerConfig testServer
    giveUp <- readTVar delay
    if (Set.null subs)
      then if giveUp
        then pure True
        else retry
      else pure giveUp

data RunningDriver = RunningDriver {
  runningDriverConfig :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
, runningDriverName :: String
}

subscribe :: TestServer -> WebDriverT IO ()
subscribe testServer = do
  liftIO $ putStrLn "Navigating to page"
  navigateTo "http://localhost:3000"
  liftIO $ putStrLn "Finding subscribe button"
  subscribeButton <- findElement XPathSelector "//button"
  liftIO $ print subscribeButton
  liftIO $ putStrLn "Clicking subscribe button"
  elementClick subscribeButton
  
  liftIO $ putStrLn "Waiting for subscription"
  delay <- liftIO $ registerDelay 10000000
  gaveUp <- liftIO $ waitForSingleSubscription delay testServer
  when gaveUp $ do
    deleteSession
    fail "Timed out waiting for subscription"
  liftIO $ putStrLn "Got subscription"
  liftIO $ appSendPushNotification (testServerConfig testServer) "test"
-- liftIO $ putStrLn "Waiting for notsification"
-- res <- executeScript "window.alert" ["hello"]
-- liftIO $ print res
  return ()