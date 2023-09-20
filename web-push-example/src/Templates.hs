{-# LANGUAGE TemplateHaskell       #-}

module Templates where

import           Data.ByteString       (ByteString)
import           Data.FileEmbed
import           Language.Haskell.TH
import           System.Directory
import qualified Text.Mustache         as Mustache
import           Text.Mustache.Compile (embedSingleTemplate)


serviceWorkerJS :: ByteString
serviceWorkerJS = $(embedFile "templates/service-worker.js")

indexHtml :: ByteString
indexHtml = $(embedFile "templates/index.html")

indexJsTemplate :: Mustache.Template
indexJsTemplate = $(embedSingleTemplate "templates/index.js.mustache")
