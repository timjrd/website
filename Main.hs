{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import System.Environment (getArgs)
import Control.Applicative ((<$>), optional)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Lite
import qualified Happstack.Server as S
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

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
            [ dir "test" $ ok $ page "route-test"
            , ok $ page "home"
            ]
-- website = msum
--           [ dir "blog"    $ Blog.main
--           , dir "code"    $ Portfolio.main
--           , dir "photos"  $ Photos.main
--           , dir "cv"      $ CV.main
--           , Blog.main
--           ]

page x = toResponse $
       H.html $ do
         --H.head $ do
         --  H.title (toHtml "test")
         H.body $ do
           p $ do
             "this is a "
             a ! href "#" $ "test"
             ", "
             x
           --body


