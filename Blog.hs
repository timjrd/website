{-# LANGUAGE OverloadedStrings
            ,ScopedTypeVariables
            #-}

module Blog where

import Shared 
import Data

import Control.Applicative ((<$>), optional)
import Control.Monad
import Text.Blaze.XHtml5
import Text.Blaze.XHtml5.Attributes
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8
import Data.Text.Lazy (pack, unpack)
import Data.List.Split
import Data.Char
import Data.Time

import Happstack.Lite
import qualified Happstack.Lite as Hap
import Network.HTTP (urlEncode, urlDecode)

import Data.Acid.Advanced   ( query', update' )
import Data.Acid

next ref = H.div ! class_ "next" $ do
  a ! href (toValue $ ref) $ "articles suivants"
  H.div ! class_ "thread deco" $ ""

prev ref = H.div ! class_ "prev" $ do
  H.div ! class_ "thread deco" $ ""
  a ! href (toValue $ ref) $ "articles précédents"


showDate x = show x

lasts db admin = postsPage 1 db admin

postsPage n db admin = do
  (ps,pre) <- query' db (BlogPage n)
  ok $ page "Blog" "blog" admin $ do
    if n > 2
      then next $ "/blog/page/" ++ (show $ n-1)
      else if n == 2
           then next ("/blog" :: String)
           else return ()
    
    forM_ ps $ \(PublishedPost i pub last p _) -> postPreviewHtml p i
                                                  (Just (pub,showDate pub))
                                                  ((\d -> (d,showDate d)) <$> last)
                                                  admin
    
    if pre
      then prev $ "/blog/page/" ++ (show $ n+1)
      else return ()


  

---- HTML Templates
postHtml :: Post -> (Maybe (UTCTime,String)) -> (Maybe (UTCTime,String)) -> Bool -> Html
postHtml p pub last edit = article ! class_ "post" $ do
  h1 $ a ! href (toValue $ "/blog/post/" ++ (show i)) $ toHtml $ postTitle p
  h2 $ toHtml $ postSubTitle p

  case pub of (Just (u,d)) -> H.div ! class_ "pubdate" $ do "publié "  ; (time' u) ! pubdate "" $ toHtml d
              Nothing      -> return ()
  case pub of (Just (u,d)) -> H.div ! class_ "last"    $ do "modifié " ; (time' u) $ toHtml d
              Nothing      -> return ()
  
  case (postTags p) of [] -> return ()
                       ts -> ul ! class_ "tags" $ forM_ ts (li . toHtml)


  if edit then aa ! href (toValue $ "/blog/edit/" ++ (show i)) $ "modifier" else return ()
  
  toHtml $ postBody p
  


postPreviewHtml :: Post -> Integer -> (Maybe (UTCTime,String)) -> (Maybe (UTCTime,String)) -> Bool -> Html
postPreviewHtml p i pub last edit = article ! class_ "post preview" ! A.id (toValue i) $ do
  H.div ! class_ "thread deco" $ ""
  case pub of (Just (u,d)) -> H.div $ do "publié "  ; (time' u) ! pubdate "" $ toHtml d
              Nothing      -> return ()
  case pub of (Just (u,d)) -> H.div $ do "modifié " ; (time' u) $ toHtml d
              Nothing      -> return ()
  
  case (postTags p) of [] -> return ()
                       ts -> ul ! class_ "tags" $ forM_ ts (li . toHtml)
                       
  H.div ! class_ "box" $ do
    H.div ! class_ "mask" $ do
      H.div ! class_ "content"   $ do
        h1 $ a ! href (toValue $ "/blog/post/" ++ (show i)) $ toHtml $ postTitle p
        h2 $ toHtml $ postSubTitle p

        if edit then aa ! href (toValue $ "/blog/edit/" ++ (show i)) $ "modifier" else return ()
  
        H.div ! class_ "desc" $ toHtml $ case (postPreview p) of (Just a) -> a
                                                                 Nothing  -> (postBody p)
      
      aside ! class_ "images" $ forM_ (postCover p) $ \(s,a) -> img
                                                                ! src (toValue s)
                                                                ! alt (toValue a)
                                                                ! A.title (toValue a)

      case (postPreview p) of (Just _) -> a ! href (toValue $ "/blog/post/" ++ (show i)) $ em "..." >> "lire la suite"
                              Nothing  -> return ()
  
  


postFormLogin p i pub last edit = do
  H.form ! action "" ! A.method "POST" ! enctype "multipart/form-data" $ do
    loginAgain
    postForm' p

  postPreviewHtml p i pub last False
  postHtml p pub last False

postForm p i pub last edit = do
  H.form ! action "" ! A.method "POST" ! enctype "multipart/form-data" $ postForm' p
  postPreviewHtml p i pub last False
  postHtml p pub last False

postForm' p = do
  H.label ! for "in_body" $ "article"
  textarea ! A.name "body" ! A.id "in_body" $ toHtml $ postSource p

  H.label ! for "in_format" $ "format"
  select ! A.name "format" ! A.id "in_format" $ do
    option "un certain format" -- en attente de Pandoc

  input ! type_ "submit" ! A.name "draft"   ! value "Sauvegarder en brouillon"
  input ! type_ "submit" ! A.name "publish" ! value "Publier"
    
