{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Shared where

import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Control.Monad.State

import Text.Blaze.XHtml5
import Text.Blaze.XHtml5.Attributes
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A

import Text.Pandoc
import Text.Pandoc.Definition
import qualified Text.Pandoc.Walk as Doc
import Text.Pandoc.Shared (stringify)

import Data.List.Split (splitOn)

import Data.Time
import Data.Time.ISO8601

import Happstack.Lite
import Happstack.Lite as Hap

---- Paths
staticDir    = "/static"   :: String
stylesheet   = "/static/style.css" :: String
faceImg      = "/static/face.jpg"  :: String
cvPdf        = "/static/timothee-jourde-cv.pdf" :: String
--imgDir = staticDir ++ "/img"


---- Utils
x <$< f = fmap f x
mToHtml x = toHtml $ case x of (Just a) -> a
                               Nothing  -> return ()

toBool a = case a of Nothing -> False
                     Just _  -> True

seeOther' :: String -> ServerPart Response
seeOther' url = seeOther url $ toResponse ()

notFound' current admin msg = notFound $ page "Introuvable" current admin $
                              H.div ! A.id "notFound" $ toHtml msg

aa = a . (H.span ! class_ "pop")
time' d = time ! datetime (toValue $ formatISO8601 d)

onlyIf c then' = if c then then' else return ()
onlyIfAuthorized c then' = if c
                           then then'
                           else unauthorized $ loginPage ""


---- Pandoc
extract doc@(Pandoc meta blocks) = ( Doc.query titles header
                                   , Doc.query infos header
                                   , Doc.query images doc
                                   , Pandoc meta <$> preview
                                   , Pandoc meta body
                                   )
    
    where (header,body) = let (h,b) = break (==HorizontalRule) blocks 
                          in  (h,tail b)

          preview = let (a,b) = break (==HorizontalRule) body
                    in if b == []
                       then Nothing
                       else Just a
    
          titles :: Block -> [String]
          titles (Header _ _ x) = [stringify x]
          titles _ = []

          images :: Inline -> [(String,String)]
          images (Image alt (url,_)) = [(url, stringify alt)]
          images _ = []

          infos :: Block -> [[String]]
          infos (BulletList l) = [stringify' <$> l]
          infos _ = []

writeHtmlString' = writeHtmlString options . tweaks
  where options = def { writerHtml5=True
                      , writerHighlight=True
                      }

tweaks :: Pandoc -> Pandoc
tweaks = Doc.walk i . Doc.walk b
    where b :: Block -> Block
          b (Header level a c) = (Header (level+1) a c)
          b t@Table{} = Div ("", ["table"], []) [t]
          b x = x

          i :: Inline -> Inline
          i (Link c t) = Link [Span ("", ["pop"], []) c] t
          i x = x

stringify' blocks = tail $ concat $ (\b -> '\n':(stringify'' b)) <$> blocks
stringify'' block = case block of
                      (Plain a)      -> stringify a
                      (Para  a)      -> stringify a
                      (RawBlock _ a) -> a
                      (Header _ _ a) -> stringify a
                      (Div _ bs)     -> stringify' bs
                      _              -> ""

-- extractBody doc = evalState (Doc.walkM f doc) 0
--   where f :: Block -> State Int Block
--         f h@(Header _ _ _) = do
--           c <- get
--           if c < 2
--             then (put $ c + 1) >> return Null
--             else return h
--         f x = return x

-- extractPreview doc = case runState (Doc.walkM f doc) False of
--   (p,True)  -> Just p
--   (_,False) -> Nothing
  
--   where f :: Block -> State Bool Block
--         f HorizontalRule = put True >> return Null
--         f x = do
--           s <- get
--           return $ if s then Null else x
  


---- Templates
page :: ToMarkup a => String -> String -> Bool -> a -> Response
page thetitle current admin thebody = toResponse $ docTypeHtml $ do
  H.head $ do
    H.title (toHtml $ "Timothée Jourde - " ++ thetitle)
    link ! rel "stylesheet" ! type_ "text/css" ! href (toValue stylesheet)
    
  body ! class_ (toValue current) $ do
    header $ do
      h1 "timothée jourde"
      h2 "site perso"
      if admin
        then aa ! A.class_ "logout button" ! href "/logout" $ "logout"
        else return ()
    
    nav $ do
      entry "/blog"   "blog"
      entry "/code"   "code"
      --entry "/photos" "photos"
      entry "/cv"     "cv"
    
    toHtml thebody

    footer ! A.id "bottom" $ ul $ do
      li $ aa ! href "" $ "à propos"
      li $ "valide " >> (aa ! href "/" $ "xhtml") >> " & " >> (aa ! href "/" $ "css")
      li $ "random kiss to " >> (aa ! href "" $ "someone")
      if not admin
        then li $ aa ! A.class_ "login"  ! href "/login"  $ "login"
        else return ()

         

  where entry :: String -> String -> Html
        entry ref name = let e = if current == name then a ! class_ "current" else a
                            in e ! href (toValue ref) $ (toHtml name)

loginPage :: String -> Response
loginPage goto = toResponse $ docTypeHtml $ do
  H.head $ do
      H.title "Timothée Jourde - Login"
      link ! rel "stylesheet" ! type_ "text/css" ! href (toValue stylesheet)

  body ! class_ "login" $ do
    H.form ! action (toValue goto) ! A.method "post" $ do
      loginInput
      input ! type_ "hidden" ! A.name "loginRedirect" ! value "true"
      input ! type_ "submit" ! A.name "submit"  ! value "give me that cookie"
  
loginInput = input ! type_ "password" ! A.name "adminPassword" ! A.id "in_pass" ! placeholder "mot de passe"
loginAgain = fieldset $ do
  legend "session expiré"
  loginInput

button' :: String -> (Html -> Html)
button' ref = aa ! class_ "button" ! href (toValue ref)

---- CV
        
cv = H.div ! A.id "cv" $ do
  H.div ! class_ "contact" $ do
    aa ! href (toValue cvPdf) ! class_ "button noprint" $ "version imprimable (PDF)"
    img ! alt "moi" ! src (toValue faceImg)
    h2 "Timothée Jourde"
    H.div $
      ul $ do
        li $ aa ! href "" $ "agivenmail"
        li "06 79 50 56 22"
    H.div $ do
      "34 rue Paul Verlaine"
      br
      "33950 Lège-Cap-Ferret"
    H.div "19 ans"

  h2 "Informatique"
  p $ do
    "Passioné, addict,"
    br
    "Pratique & (auto)apprentissage actif depuis le collège !"

  table ! class_ "compact" $ do
    caption "technos."
    row [ "C"                   , "bon"             ]
    row [ "C++"                 , "bon"             ]
    row [ "Haskell"             , "débutant motivé" ]
    row [ "XHTML & CSS"         , "bon"             ]
    row [ "Objective-C / Cocoa" , "notions"         ]
    row [ "C# / .NET"           , "notions"         ]
    row [ "SQL"                 , "pas mal"         ]
  
  p $ do
    "Bonne capacités de modélisation/abstraction, UML & MCD."
    br
    "Libriste, utilisation/bricolage GNU/Linux."

  table $ do
    caption $ do
      "quelques réalisations"
      aa ! class_ "noprint more" ! href "/code" $ " + de détails et autres projets"
        
    row [ "Jeux 2D : clone Puru Puru Digger, Guitar Hero simplifié, clone Pudding Monsters, Copter..."
        , "C++/SFML, Objective-C/Cocoa, C/SDL"
        , "binômes et seul"
        , "études et perso" ]
        
    row [ "Parser & machine virtuelle pour un language d'assemblage abstrait haut niveau, sur le principe du jeu CoreWar (but pédagogique)"
        , "C++"
        , "binôme"
        , "associatif" ]
        
    row [ "Site web perso dynamique (gestion blog ad hoc)"
        , "Haskell, XHTML/CSS"
        , "seul"
          , "perso" ]

  p "Curieux, culture appronfondie dans le domaine."

  H.div ! class_ "half" $ do
    h3 "Emplois & Stages"
    ul ! class_ "box" $ do
      li "Agent de nettoyage (saisonnier)"
      li $ "Stage de 3" >> sup "ème" >> " dans une clinique (CTO à Nancy)"

    h3 "Anglais"
    p ! class_ "box" $ "Bon niveau générale lu, écrit, parlé. Habitué à lire des documents techniques."

  H.div ! class_ "half" $ do
    h3 "Études & Diplômes"
    H.div ! class_ "box" $ do
      p $ "Entre en 2" >> sup "ème" >> "année de DUT INFO à Bordeaux"
      ul $ do
        li "BAC S spé. Maths section européenne Anglais mention AB"
        li "Permis B"
        li "Niveau 1 de plongée"

  ul ! class_ "box" $ do
    li "Musicien : batteur dans un groupe, un peu de piano"
    li "Ski alpin, bon niveau"
    li "Voyages à : New-York, Londres, Gabon, Sénégal..."
        
      
  where row :: [String] -> Html
        row xs = tr $ forM_ xs (\x -> td (toHtml x))
    
