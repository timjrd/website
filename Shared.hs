{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Shared where

import Control.Monad
import Text.Blaze.XHtml5
import Text.Blaze.XHtml5.Attributes
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A

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

seeOther' :: String -> String -> ServerPart Response
seeOther' url desc = seeOther url $ toResponse desc

notFound' current admin msg = notFound $ page "Introuvable" current admin $
                              H.div ! A.id "notFound" $ toHtml msg

aa = a . (H.span ! class_ "pop")
time' d = time ! datetime (toValue $ formatISO8601 d)

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
        then aa ! A.class_ "logout" ! href "/logout" $ "logout"
        else return ()
    
    nav $ do
      entry "/blog"   "blog"
      entry "/code"   "code"
      entry "/photos" "photos"
      entry "/cv"     "cv"
    
    toHtml thebody

    footer $ ul $ do
      li $ aa ! href "" $ "à propos"
      li $ "valide " >> (aa ! href "/" $ "xhtml") >> " & " >> (aa ! href "/" $ "css")
      li $ "rand" >> (H.em ! class_ "heart" $ "<3") >> "m kiss to " >> (aa ! href "" $ "someone")
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
    
