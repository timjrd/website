{-# LANGUAGE OverloadedStrings
            ,ScopedTypeVariables
            #-}

module Code where

import Shared
import Data

import Control.Applicative ((<$>), optional)
import Control.Monad

import Text.Blaze.XHtml5
import Text.Blaze.XHtml5.Attributes
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8

import Text.Pandoc

import Data.Text.Lazy (pack, unpack)
import Data.List.Split
import Data.Char
--import Codec.Text.IConv

import Happstack.Lite
import qualified Happstack.Lite as Hap
import Network.HTTP (urlEncode, urlDecode)

import Data.Acid.Advanced   ( query', update' )
import Data.Acid


---- HTML Templates

editBar i = H.div ! class_ "button-bar" $ do
  button' ("/code/edit/"      ++ (urlEncode i)) "modifier"
  button' ("/code/unpublish/" ++ (urlEncode i)) "retirer"

projectHtml :: Project -> String -> Bool -> Html
projectHtml p i edit = article ! class_ "box more project" ! A.id (toValue $ urlEncode $ i) $ do
  H.div ! class_ "mask" $ do
    H.div ! class_ "content"   $ do
      h1 $ toHtml $ projectName p
      h2 $ toHtml $ kind p

      onlyIf edit $ editBar i
  
      H.div  ! class_ "body"    $ preEscapedToHtml $ desc p

      forM_ (tags p) $ \x -> 
        H.span ! class_ "info" $ toHtml x
      
      br
      
      onlyIf ((mainTechs  p) /= [] || (otherTechs  p) /= []) $
        ul ! class_ "info" $ do
          forM_ (mainTechs  p) (\x -> li $ em $ toHtml x)
          forM_ (otherTechs p) (\x -> li $ toHtml x)
      
    aside ! class_ "images" $ forM_ (images p) (\(s,a) -> img
                                                          ! src (toValue s)
                                                          ! alt (toValue a)
                                                          ! A.title (toValue a))

  onlyIf ((more p) /= []) $ H.div ! class_ "more" $ do
    forM_ (more p) $ \(ref,str) -> 
      aa ! class_ "button" ! href (toValue ref) $ toHtml str


projectForm p = do
  H.form ! action "" ! A.method "POST" ! enctype "multipart/form-data" $ projectForm' p
  projectHtml p "" False

projectFormLogin p = do
  H.form ! action "" ! A.method "POST" ! enctype "multipart/form-data" $ do
    loginAgain
    projectForm' p
    
  projectHtml p "" False

  
projectForm' p = do
    textarea ! A.name "content" ! A.id "in_content" $ toHtml $ Data.source p
    br
    input ! type_ "submit" ! A.name "draft"   ! value "Sauvegarder en brouillon"
    input ! type_ "submit" ! A.name "publish" ! value "Publier"
    

---- Code
published db admin = do
  Hap.method GET
  ps <- query' db PublishedProjects
  ok $ page "Code" "code" admin $ do
    onlyIf admin $ H.div ! class_ "button-bar center" $ do
      button' "/code/new"    "Nouveau"
      button' "/code/drafts" "Voir les brouillons"

    forM_ ps $ \(Project' i (Published p _)) -> projectHtml p i admin

drafts db admin = onlyIfAuthorized admin $ do
  ps <- query' db ProjectDrafts
  ok $ page "Code" "code" admin $
    forM_ ps $ \(Project' i (Draft p)) -> projectHtml p i admin

unpublish i db admin = onlyIfAuthorized admin $ do
  update' db (UnpublishProject i)
  seeOther' $ "/code/edit/" ++ (urlEncode i)


viewForm :: (Maybe String) -> (AcidState DataBase) -> Bool -> ServerPart Response
viewForm id_ db admin = do
  Hap.method GET
  if not admin
    then unauthorized $ loginPage ""
            
    else case id_ of Nothing  -> ok $ page "Édition" "code" admin (projectForm $ parse demoProject)
                     (Just i) -> do
                       pr <- query' db (EditProject i)
                       case pr of Nothing  -> notFound' "code" admin ("404 :)" :: String)
                                  (Just p) -> ok $ page "Édition" "code" admin (projectForm p)

processForm id_ db admin = do
  Hap.method POST
  publish <- toBool <$> (optional $ lookText "publish")
  p <- parse <$> unpack <$> lookText "content"
  if not admin
    then unauthorized $ page "Édition" "code" admin (projectFormLogin p)
                 
    else if publish
         then do i <- case id_ of Nothing  -> update' db $ PublishNewProject 0 p
                                  (Just i) -> update' db $ PublishProject i Nothing p
                 seeOther' $ "/code#" ++ (urlEncode i)
                                                        
         else do i <- case id_ of Nothing  -> update' db $ DraftNewProject 0 p
                                  (Just i) -> update' db $ DraftProject i Nothing p
                 seeOther' $ "/code/edit/" ++ (urlEncode i)
          

parse source = 
  let (titles, infos, more, images', _, body) = extract $ readOrg def $ filter (/='\r') source

      (name'    : kind'                    :_) = titles        ++ repeat "sans titre"
      (tags'    : mainTechs' : otherTechs' :_) = infos         ++ repeat []

  in Project
     name'
     kind'
     (writeHtmlString' body)
     source
     "Emacs Org mode"
     tags'
     mainTechs'
     otherTechs'
     images'
     more


readListWith d = filter (/="") . fmap (unwords . words) . splitOn d
ifNotBlank s = if all (isSpace) s then Nothing else Just s

