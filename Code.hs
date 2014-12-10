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
import Data.List (partition)
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
projectHtml p i edit =
  let classes = toValue $ "box more project" ++ if (tinyProject p) then " tiny" else "" :: String
      contentDiv' = H.div ! class_ "content"
      contentDiv  = if (images p) == [] then contentDiv' ! A.style "width: 100%;" else contentDiv'
      
  in article ! class_ classes ! A.id (toValue $ urlEncode $ i) $ do
    H.div ! class_ "mask" $ do
      contentDiv $ do
        h1 $ toHtml $ projectName p
        h2 $ toHtml $ kind p

        onlyIf edit $ editBar i

        H.div  ! class_ "body"    $ preEscapedToHtml $ desc p


        let tagsNotEmpty  = (tags p) /= []
            techsNotEmpty = ((mainTechs  p) /= [] || (otherTechs  p) /= [])

        onlyIf (tagsNotEmpty || techsNotEmpty) $ H.div !class_ "infos" $ do
          forM_ (tags p) $ \x -> 
            H.span (toHtml x)

          onlyIf (tagsNotEmpty && techsNotEmpty) $ br

          onlyIf techsNotEmpty $ do
            ul $ do
              forM_ (mainTechs  p) (\x -> li $ em $ toHtml x)
              forM_ (otherTechs p) (\x -> li $ toHtml x)

      onlyIf ((images p) /= []) $
        aside ! class_ "images" $ forM_ (images p) (\(s,a) -> img
                                                              ! src (toValue s)
                                                              ! alt (toValue a)
                                                              ! A.title (toValue a))

    onlyIf ((more p) /= []) $ H.div ! class_ "more" $ do
      forM_ (more p) $ \(ref,str) -> 
        aa ! class_ "button" ! href (toValue ref) $ toHtml str


projectForm ids p = do
  H.form ! action "" ! A.method "POST" ! enctype "multipart/form-data" $ projectForm' ids p
  projectHtml p "" False

projectFormLogin ids p = do
  H.form ! action "" ! A.method "POST" ! enctype "multipart/form-data" $ do
    loginAgain
    projectForm' ids p
    
  projectHtml p "" False


projectForm' ids p = do
    textarea ! A.name "content" ! A.id "in_content" $ toHtml $ Data.source p
    br
    H.label ! for "in_position" $ "placer après "
    select ! A.name "position" ! A.id "in_position" $ do
      option ! value "0" $ "(placer en premier)"
      forM_ (zip [1..] ids) (\(n,i) -> (option ! value (toValue (n :: Int)) $ toHtml i))
                                        
    " "
    H.label ! for "in_tiny" $ "petit truc"
    let checkbox = input ! type_ "checkbox" ! A.name "tiny" ! A.id "in_tiny"
    if (tinyProject p)
      then checkbox ! checked "checked"
      else checkbox 
    br
    input ! type_ "submit" ! A.name "draft"   ! value "Sauvegarder en brouillon"
    input ! type_ "submit" ! A.name "publish" ! value "Publier"
    

---- Code
published db admin = do
  Hap.method GET
  ps <- query' db PublishedProjects
  
  let (tinyProjects, projects) =
        partition (\(Project' _ (Published p _)) -> tinyProject p) ps
  
  ok' $ page "Code" "code" admin $ do
    onlyIf admin $ H.div ! class_ "button-bar center" $ do
      button' "/code/new"    "Nouveau"
      button' "/code/drafts" "Voir les brouillons"

    p ! class_ "welcome" $ "Bienvenue à toi chère internaute manifestement égaré dans les méandres du web. Tu ne me connais peut-être pas, mais voici les principaux projets que j'ai dévellopés ou auquels j'ai contribué."
    forM_ projects     $ \(Project' i (Published p _)) -> projectHtml p i admin

    p ! class_ "welcome" $ "Et si tu t'ennuie tu sera j'en suis sur ravis de trouver ci-après quelques autres petits projets ou réalisations."
    H.div ! class_ "tiny-projects" $ 
      forM_ tinyProjects $ \(Project' i (Published p _)) -> projectHtml p i admin

drafts db admin = onlyIfAuthorized admin $ do
  ps <- query' db ProjectDrafts
  ok' $ page "Code" "code" admin $
    forM_ ps $ \(Project' i (Draft p)) -> projectHtml p i admin

unpublish i db admin = onlyIfAuthorized admin $ do
  update' db (UnpublishProject i)
  seeOther' $ "/code/edit/" ++ (urlEncode i)


viewForm :: (Maybe String) -> (AcidState DataBase) -> Bool -> ServerPart Response
viewForm id_ db admin = do
  Hap.method GET
  if not admin
    then unauthorized $ loginPage "" else do
    
    ids <- query' db (ProjectIds)
    case id_ of
      Nothing  -> ok' $ page "Édition" "code" admin (projectForm ids $ parse False demoProject)
      (Just i) -> do
        pr <- query' db (EditProject i)
        case pr of Nothing  -> notFound' "code" admin ("404 :)" :: String)
                   (Just p) -> ok' $ page "Édition" "code" admin (projectForm ids p)

processForm id_ db admin = do
  Hap.method POST
  publish <- toBool <$> (optional $ lookText "publish")
  tiny <- toBool <$> (optional $ lookText "tiny")
  pos <- read <$> unpack <$> lookText "position"
  p <- (parse tiny) <$> unpack <$> lookText "content"
  if not admin
    then do
    ids <- query' db (ProjectIds)
    unauthorized' $ page "Édition" "code" admin (projectFormLogin ids p)
                 
    else if publish
         then do i <- case id_ of Nothing  -> update' db $ PublishNewProject pos p
                                  (Just i) -> update' db $ PublishProject i (Just pos) p
                 seeOther' $ "/code#" ++ (urlEncode i)
                                                        
         else do i <- case id_ of Nothing  -> update' db $ DraftNewProject pos p
                                  (Just i) -> update' db $ DraftProject i (Just pos) p
                 seeOther' $ "/code/edit/" ++ (urlEncode i)
          

parse tiny source = 
  let (titles, infos, more, images', _, body) = extract $ readOrg def $ filter (/='\r') source

      (name'    : kind'                    :_) = titles        ++ repeat ""
      (tags'    : mainTechs' : otherTechs' :_) = infos         ++ repeat []

  in Project
     name'
     kind'
     (writeHtmlString' body)
     source
     "Emacs Org mode"
     tiny
     tags'
     mainTechs'
     otherTechs'
     images'
     more


readListWith d = filter (/="") . fmap (unwords . words) . splitOn d
ifNotBlank s = if all (isSpace) s then Nothing else Just s

