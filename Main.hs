{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import System.Environment (getArgs)
import Control.Applicative ((<$>), optional)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Lite
import qualified Happstack.Server as S
import Text.Blaze.XHtml5 (docTypeHtml, Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.XHtml5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A

import Templates
-- import qualified Blog
-- import qualified Code
-- import qualified Portfolio
-- import qualified Photos
-- import qualified CV

main = do
  [host,port] <- getArgs
  s <- S.bindIPv4 host (read port)
  S.simpleHTTPWithSocket s (S.nullConf {S.port=(read port)}) $ website

website :: ServerPart Response
website = msum
          [ dir "blog"    $ tmp
          , dir "code"    $ tmp
          , dir "photos"  $ tmp
          , dir "cv"      $ tmp
          , dir "static"  $ static
          , tmp
          ]

tmp = ok $ toResponse $ page "home" [] ""

static = serveDirectory EnableBrowsing [] "."
