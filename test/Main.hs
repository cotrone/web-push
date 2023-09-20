module Main where

import WebPushEncryptionSpec
import WebPushMock
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  encryptionSpec <- testSpecs spec
  defaultMain $
    testGroup "Tests" [
        testGroup "WebPushEncryptionSpec" encryptionSpec
      , testGroup "Mock" [testSendMessage]
    ]