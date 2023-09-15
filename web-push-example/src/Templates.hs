{-# LANGUAGE TemplateHaskell       #-}

module Templates where

import qualified Text.Mustache              as Mustache
import           Text.Mustache.Compile      (embedSingleTemplate)
import           Data.FileEmbed
import System.Directory
import Language.Haskell.TH

serviceWorkerTemplate :: Mustache.Template
serviceWorkerTemplate = $(embedSingleTemplate "templates/service-worker.js.mustache")

indexTemplate :: Mustache.Template
indexTemplate = $(embedSingleTemplate "templates/index.html.mustache")

indexJsTemplate :: Mustache.Template
indexJsTemplate = $(embedSingleTemplate "templates/index.js.mustache")
