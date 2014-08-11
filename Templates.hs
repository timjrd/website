{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Templates where

import Control.Monad

import Text.Blaze.XHtml5
import Text.Blaze.XHtml5.Attributes
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A

data Stylesheet = Stylesheet String (Maybe String) -- href media

page thetitle stylesheets thebody = docTypeHtml $ do
  H.head $ do
    H.title thetitle
    forM_ stylesheets (\(Stylesheet ref m) -> let l = link
                                                      ! rel   "stylesheet"
                                                      ! type_ "text/css"
                                                      ! href  (toValue ref)
                                                      
                                              in case m of (Just s) -> l ! media (toValue s)
                                                           Nothing  -> l)
    
  body $ do
    header $ do
      h1 "timothée jourde"
      h2 "site perso"
    
    nav $ do
      entry "/blog"   "blog"
      entry "/code"   "code"
      entry "/photos" "photos"
      entry "/cv"     "cv"
    
    thebody

  where entry thehref content = do
          a ! href thehref $ content
          " "

        
        

