{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Shared
import Data
import qualified Blog
import qualified Code
import qualified Cv
-- import qualified Photos

import System.Environment (getArgs, getEnv)
import Happstack.Lite
import qualified Happstack.Server as S

import Data.Acid            ( AcidState, Query, Update
                            , makeAcidic, openLocalStateFrom )
import Data.Acid.Advanced   ( query', update' )
import Data.Acid.Local      ( createCheckpointAndClose )

import Control.Applicative (optional)
import Control.Exception.Base (bracket)
import Control.Monad
import Control.Monad.IO.Class

import Crypto.PasswordStore
import Data.ByteString.Char8  (pack, unpack)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import Data.Time
import Data.Text.Lazy.Encoding (encodeUtf8)

main = do
  [host,p,datadir] <- getArgs
  bracket (openLocalStateFrom (datadir++"/database")  initialDataBase) (createCheckpointAndClose) $ \db -> do
    let port = read p
    s <- S.bindIPv4 host port
    S.simpleHTTPWithSocket s (S.nullConf {S.port=port}) $ do
      S.decodeBody (S.defaultBodyPolicy "/tmp/" 4096 15000 4096)
      website datadir db


--website :: ServerPart Response
website datadir db = do
  admin <- authenticate (datadir ++ "/PASSWORD") db
  r <- loginRedirect
  if r
    then seeOther' ""

    else msum [ dir "blog"    $ msum [ dir "page"      $ path $ \n -> Blog.viewBlogPage n db admin
                                     , dir "post"      $ path $ \i -> Blog.viewPost i db admin
                                     , dir "drafts"    $ Blog.drafts db admin
                                     , dir "unpublish" $ path $ \i -> Blog.unpublish i db admin
                                     , dir "edit"      $ path $ \i -> msum [ Blog.viewForm    (Just i) db admin
                                                                           , Blog.processForm (Just i) db admin
                                                                           ]
                                       -- new devrais seulement créer le post et rediriger vers /edit
                                     , dir "new"  $ msum [ Blog.viewForm    Nothing db admin
                                                         , Blog.processForm Nothing db admin
                                                         ]
                                     , Blog.lasts db admin
                                     ]
                
              , dir "code"    $ msum [ dir "edit" $ path (\i -> msum [ Code.viewForm    (Just i) db admin
                                                                     , Code.processForm (Just i) db admin
                                                                     ])
                                     , dir "new" $ msum [ Code.viewForm    Nothing db admin
                                                        , Code.processForm Nothing db admin
                                                        ]
                                     , dir "drafts"    $ Code.drafts db admin
                                     , dir "unpublish" $ path $ \i -> Code.unpublish i db admin

                                     , Code.published db admin
                                     ]
                
              --, dir "photos"  $ ok $ page "Photos" "photos" admin ""
              , dir "cv"      $ ok $ page "CV" "cv" admin Cv.cvHtml
              , dir "static"  $ serveStatic (datadir ++ staticDir)
              , dir "login"   $ ok $ loginPage "/"
              , dir "logout"  $ (update' db CloseSession) >> Blog.lasts db False

              , Blog.lasts db admin
              ]

  

serveStatic = serveDirectory EnableBrowsing []



openSession :: AcidState DataBase -> ServerPart ()
openSession db = do
  now <- liftIO getCurrentTime
  let expiry = addUTCTime (50*60) now

  key  <- liftIO $ genKey 10
  hash <- liftIO $ makePassword (pack key) 15

  update' db $ UpdateSession $ Data.Session hash expiry

  let ck = ((mkCookie "sessionKey" key) {secure=False, httpOnly=True})
  addCookies [(MaxAge (50*60), ck)]

loginRedirect = do
  r <- optional $ lookText "loginRedirect"
  case r of Nothing -> return False
            Just _  -> return True

authenticate pwfile db = do
  cok <- cookieAuth db
  fok <- formAuth pwfile db
  return (cok || fok)

formAuth pwfile db = do
  mPass <- optional $ lookText "adminPassword"
  case mPass of Nothing   -> return False
                Just pass -> do
                  hash <- liftIO $ BS.readFile pwfile
                  if verifyPassword (toStrict $ encodeUtf8 pass) hash
                    then do openSession db
                            return True
                         
                    else return False

                  

cookieAuth db = do
  mKey <- optional $ lookCookieValue "sessionKey"
  case mKey of Nothing  -> return False
               Just key -> do
                 s <- query' db GetSession
                 case s of Closed                     -> return False
                           (Data.Session hash expiry) -> do
                             now <- liftIO getCurrentTime
                             if now > expiry
                               then update' db CloseSession >> return False
                               
                               else if verifyPassword (pack key) hash
                                    then if now > (addUTCTime (-40) expiry)
                                         then openSession db >> return True
                                         else return True
                                      
                                    else return False
  


  
genKey n = do
  rs <- replicateM n (genSaltIO)
  return $ concat $ map (unpack . exportSalt) rs
  

--echo = ok . toResponse
