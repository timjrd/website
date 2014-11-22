{-# LANGUAGE OverloadedStrings #-}

module Cv where

import Shared

import Text.Blaze.XHtml5
import Text.Blaze.XHtml5.Attributes
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A

cvHtml = H.div ! class_ "cv-body" $ do
  h1 ! class_ "print" $ "me-timjrd.rhcloud.com/cv"
  aa ! href (toValue cvPdf) ! class_ "button noprint" $ "version imprimable (PDF)"
  H.div ! class_ "contact" $ do
      img ! src "static/face.jpg" ! alt "me"
      h2 "Timothée Jourde"
      h3 "étudiant en informatique"
      H.div $ do
          aa ! href "mailto:itim.lcf@gmail.com" $ "itim.lcf@gmail.com"
          br
          "06 79 50 56 22"
      H.div $ do
          "34 rue Paul Verlaine"
          br
          "33950 Lège-Cap-Ferret"
      H.div $ do
          "19 ans"
          br
          "Permis B"
  h2 "Informatique"
  p $ do
      "Passionné, addict,"
      br
      "pratique & (auto)apprentissage actif depuis le collège !"
  h4 "compétences"
  H.div ! class_ "half" $ ul ! class_ "tiny-box light" $ do
      li "C"
      li "C++"
      li "Haskell"
      li "XHTML & CSS"
      li "Objective‑C / Cocoa"
      li "C# / .NET"
      li "SQL"
  H.div ! class_ "half" $ ul $ do
      li "Emacs, Eclipse, Visual Studio, Git"
      li "Utilisation/bricolage GNU/Linux : bonne connaissance"
      li "Formation en Réseaux"
      li "Formation en Architecture (assembleur PowerPC)"
      li "Conaissances SGBD Oracle, SQL Server"
      li "Formalismes UML & MCD. Bonnes capacités d'abstraction/modélisation."
      li "Formation en Gestion de Projets & méthodes Agiles"
  table $ do
      caption "quelques réalisations"
      tr $ do
          th "en cours"
          td "Site web perso. dynamique, gestion blog ad-hoc et design from scratch"
          td "Haskell, Happstack, XHTML/CSS"
          td "seul"
          td "perso."
      tr $ do
          th "juillet 2014"
          td "Parser & machine virtuelle pour un langage d'assemblage haut niveau simple, sur le principe du jeu CoreWar (but pédagogique)"
          td "C++"
          td "binôme"
          td "associatif"
      tr $ do
          th "2009 - 2014"
          td "Jeux 2D : clone Puru Puru Digger, Guitar Hero simplifié, clone Pudding Monsters, Copter..."
          td "C++/SFML, Objective‑C/Cocoa, C/SDL"
          td "binômes et seul"
          td "études et perso."
  p $ do
      "Curieux, culture appronfondie dans le domaine."
      br
      "Adepte du logiciel libre."
  H.div ! class_ "half" $ do
      h3 "Formation"
      table $ do
          tr $ do
              th "2013 - 2015"
              td $ do
                  "DUT INFO à l'IUT de Bordeaux (actuellement en 2"
                  sup "ème"
                  "année)"
          tr $ do
              th "2012 - 2013"
              td "BAC S spé. Maths section européenne Anglais mention AB"
      h3 "Anglais"
      p ! class_ "tiny-box" $ "Bon niveau général lu, écrit, parlé. Habitué à lire des documents techniques."
  -- 
  -- 
  --     
  H.div ! class_ "half" $ do
      h3 "Emplois & Stages"
      ul ! class_ "tiny-box" $ do
          li "Agent de nettoyage (saisonnier)"
          li $ do
              "Stage de 3"
              sup "ème"
              "dans une clinique (CTO à Nancy)"
      ul ! class_ "tiny-box" $ do
          li "Musicien : batteur dans un groupe, un peu de piano"
          li "Ski alpin, bon niveau"
          li "Niveau 1 de plongée"
          li "Voyages : Gabon, Sénégal, New-York, Europe"
