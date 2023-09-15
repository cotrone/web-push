module Main where

import WebPushExample

main :: IO ()
main = do
  cfg <- initPersistentConfig
  runExampleApp 3000 cfg
