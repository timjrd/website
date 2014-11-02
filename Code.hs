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
      H.span ! class_ "info"    $ toHtml $ context p
      H.span ! class_ "info"    $ toHtml $ role p
      br
      ul ! class_ "info" $ do
        forM_ (mainTechs  p) (\x -> li $ em $ toHtml x)
        forM_ (otherTechs p) (\x -> li $ toHtml x)
      
    aside ! class_ "images" $ forM_ (images p) (\(s,a) -> img
                                                          ! src (toValue s)
                                                          ! alt (toValue a)
                                                          ! A.title (toValue a))

  H.div ! class_ "more" $ do
    case (moreDetails p) of Nothing -> return ()
                            Just d  -> aa ! class_ "button" ! href (toValue d) $ em "+ d'infos" >> " / voir les sources"
  
    case (download p) of Nothing -> return ()
                         Just d  -> aa ! class_ "button" ! href (toValue d) $ "télécharger"


projectForm p = do
  H.form ! action "" ! A.method "POST" ! enctype "multipart/form-data" $ projectForm' p
  projectHtml p "" False

projectFormLogin p = do
  H.form ! action "" ! A.method "POST" ! enctype "multipart/form-data" $ do
    loginAgain
    projectForm' p
    
  projectHtml p "" False

  
projectForm' p = do
    -- H.label ! for "in_name" $ "nom du projet"
    -- input ! type_ "text" ! A.name "name" ! A.id "in_name" ! value (toValue $ projectName p)
    
    -- H.label ! for "in_kind" $ "catégorie/type de projet"
    -- input ! type_ "text" ! A.name "kind" ! A.id "in_kind" ! value (toValue $ kind p)

    --H.label ! for "in_desc" $ "description"
    textarea ! A.name "content" ! A.id "in_content" $ toHtml $ Data.source p

    -- H.label ! for "in_format" $ "format de la description"
    -- select ! A.name "format" ! A.id "in_format" $ do
    --   option "un certain format" -- en attente de Pandoc

    -- H.label ! for "in_context" $ "contexte/cadre"
    -- input ! type_ "text" ! A.name "context" ! A.id "in_context" ! value (toValue $ context p)

    -- H.label ! for "in_role" $ "rôle"
    -- input ! type_ "text" ! A.name "role" ! A.id "in_role" ! value (toValue $ role p)

    -- H.label ! for "in_mainTechs" $ "technos principales"
    -- input ! type_ "text" ! A.name "mainTechs" ! A.id "in_mainTechs" ! value
    --   (toValue . concat . fmap (\x -> x ++ ", ") $ mainTechs p)

    -- H.label ! for "in_otherTechs" $ "technos secondaires"
    -- input ! type_ "text" ! A.name "otherTechs" ! A.id "in_otherTechs" ! value
    --   (toValue . concat . fmap (\x -> x ++ ", ") $ mainTechs p)

    -- H.label ! for "in_images" $ "images"
    -- textarea ! A.name "images" ! A.id "in_images" $
    --   toHtml . concat . fmap (\(s,a) -> s ++ "\n" ++ a ++ "\n\n") $ images p

    -- H.label ! for "in_more" $ "URL à consulter pour + d'infos"
    -- input ! type_ "text" ! A.name "more" ! A.id "in_more" ! value (toValue $ context p)

    -- H.label ! for "in_more" $ "URL vers téléchargement"
    -- input ! type_ "text" ! A.name "download" ! A.id "in_download" ! value (toValue $ context p)

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
  let (titles, infos, images', _, body) = extract $ readOrg def $ filter (/='\r') source

      (name'    : kind'                         :_) = titles        ++ repeat "sans titre"
      (cr       : mainTechs' : otherTechs' : md :_) = infos         ++ repeat []
      (context' : role'                         :_) = cr            ++ repeat "non renseigné"
      (more'    : download'                     :_) = (Just <$> md) ++ repeat Nothing

  in Project
     name'
     kind'
     (writeHtmlString' body)
     source
     "Emacs Org mode"
     context'
     role'
     mainTechs'
     otherTechs'
     images'
     more'
     download'


readListWith d = filter (/="") . fmap (unwords . words) . splitOn d
ifNotBlank s = if all (isSpace) s then Nothing else Just s

