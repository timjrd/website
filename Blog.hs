{-# LANGUAGE OverloadedStrings
            ,ScopedTypeVariables
            #-}

module Blog where

import Shared 
import Data

import Control.Applicative ((<$>), optional)
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import qualified Control.Monad.Writer as Mo
import Control.Monad.IO.Class
import Text.Pandoc
import qualified Text.Pandoc.Walk as Doc
import Text.Pandoc.Shared (stringify)
import Text.Blaze.XHtml5
import Text.Blaze.XHtml5.Attributes
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8
import Data.Text.Lazy (pack, unpack)
import Data.List.Split
import Data.Char
import Data.Time
import Data.Time.Format.Human
import System.Locale

import Happstack.Lite
import qualified Happstack.Lite as Hap
import qualified Happstack.Server.Response as Hap
import Network.HTTP (urlEncode, urlDecode)

import Data.Acid.Advanced   ( query', update' )
import Data.Acid


showMDate ct = fmap $ \d -> (d, humanReadableTimeI18N' humanTimeLocale_FR ct d)

humanTimeLocale_FR :: HumanTimeLocale
humanTimeLocale_FR = HumanTimeLocale
    { justNow       = "à l'instant"
    , secondsAgo    = \x -> "il y a " ++ x ++ " secondes"
    , oneMinuteAgo  = "il y a une minute"
    , minutesAgo    = \x -> "il y a " ++ x ++ " minutes"
    , oneHourAgo    = "il y a une heure"
    , aboutHoursAgo = (\x -> "il y a environ " ++ x ++ " heures")
    , at            = \_ -> ("le "++)
    , daysAgo       = \x -> "il y a " ++ x ++ " jours"
    , weekAgo       = \x -> "il y a " ++ x ++ " semaine"
    , weeksAgo      = \x -> "il y a " ++ x ++ " semaines"
    , onYear        = ("le " ++)
    , locale        = timeLocale_FR
--    , timeZone      = utc
    , dayOfWeekFmt  = "%l:%M %p à %A"
    , thisYearFmt   = "%e %B"
    , prevYearFmt   = "%e %B %Y"
    }
    

timeLocale_FR :: TimeLocale
timeLocale_FR = TimeLocale {
  wDays  = [("dimanche", "dim"),  ("lundi",    "lun"),
            ("mardi"   , "mar"),  ("mercredi", "mer"),
            ("jeudi"   , "jeu"),  ("vendredi", "ven"),
            ("samedi"  , "sam")],

  months = [("janvier"  , "jan"), ("fevrier" ,  "fev"),
            ("mars"     , "mar"), ("avril"    ,  "avr"),
            ("mai"      , "mai"), ("juin"    ,  "jun"),
            ("juillet"  , "jul"), ("aout"    ,  "aug"),
            ("septembre", "sep"), ("octobre" ,  "oct"),
            ("novembre" , "nov"), ("décembre",  "déc")],

  intervals = [ ("année","années")
              , ("moi", "mois")
              , ("jour","jours")
              , ("heure","heures")
              , ("min","mins")
              , ("sec","secs")
              , ("usec","usecs")
              ],

  amPm = ("AM", "PM"),
  dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y",
  dateFmt = "%d/%m/%y",
  timeFmt = "%H:%M:%S",
  time12Fmt = "%I:%M:%S %p"
  }
                             

viewPost i db admin = do
  mp <- query' db (GetPost i)
  ct <- liftIO $ getCurrentTime
  case mp of
    Nothing  -> notFound' "blog" admin ("404 :)" :: String)
    (Just (PublishedPost _ pub last p _)) ->
      ok $ page "Blog" "blog" admin $
      postHtml p
      (showMDate ct $ Just pub)
      (showMDate ct last)
      admin
      

lasts db admin = do
  p <- viewBlogPage' 1 db admin
  ok $ page "Blog" "blog" admin $ p

viewBlogPage' n db admin = do
  (ps,pre) <- query' db (BlogPage n)
  ct <- liftIO $ getCurrentTime
  
  return $ do
    if n > 2
      then next $ "/blog/page/" ++ (show $ n-1)
      else if n == 2
           then next ("/blog" :: String)
           else return ()
    
    forM_ ps $ \(PublishedPost i pub last p _) -> postPreviewHtml p i
                                                  (showMDate ct $ Just pub)
                                                  (showMDate ct last)
                                                  admin
    
    if pre
      then prev $ "/blog/page/" ++ (show $ n+1)
      else return ()



viewForm mi db admin = do
  Hap.method GET
  if not admin then (unauthorized $ loginPage "") else do
    case mi of
      Nothing -> ok $ page "Édition" "blog" admin $ postForm demoPost 0 Nothing Nothing
      (Just i) -> do
        p'' <- query' db (GetPost i)
        case p'' of
          Nothing   -> notFound' "blog" admin ("404 :)" :: String)
          (Just p') -> do
            let (p,pub,last) = case p' of (PublishedPost _ pub last p Nothing)  -> (p,(Just pub),last)
                                          (PublishedPost _ pub last _ (Just p)) -> (p,(Just pub),last)
                                          (PostDraft     _ p)                   -> (p,Nothing,Nothing)

            ct <- liftIO $ getCurrentTime
            ok $ page "Édition" "blog" admin $ postForm p i (showMDate ct pub) (showMDate ct last)
  
  
processForm mi db admin = do
  Hap.method POST
  ct <- liftIO $ getCurrentTime
  publish <- toBool <$> (optional $ lookText "publish")
  (p,err) <- runWriter <$> readForm
  case err of
    (_:_) -> Hap.badRequest $ page "Édition" "blog" admin (postForm p 0 Nothing Nothing)
    []    -> 
      if not admin
      then unauthorized $ page "Édition" "blog" admin (postFormLogin p 0 Nothing Nothing)
      else if publish
           then do i <- case mi of Nothing  -> update' db $ PublishNewPost p ct
                                   (Just i) -> update' db $ PublishPost i p ct
                   seeOther' ("/blog/post/" ++ (show i)) "publish: after POST, redirect GET"
                
           else do i <- case mi of Nothing  -> update' db $ DraftNewPost p
                                   (Just i) -> update' db $ DraftPost i p
                   seeOther' ("/blog/edit/" ++ (show i)) "save draft: after POST, redirect GET"


    

readForm :: ServerPart (Mo.Writer [String] Post)
readForm = do
  doc'   <- unpack <$> lookText "content"
  format <- unpack <$> lookText "format"
  let doc     = readOrg def $ filter (/='\r') doc'
      cover   = take 4 $ extractImages doc
      body'   = extractBody doc
      preview = writeHtmlString def {writerHtml5=True} <$> extractPreview body'
      body    = writeHtmlString def {writerHtml5=True} body'
      titles  = case take 2 $ extractHeaders doc of []    -> tell ["titre manquant"] >> return ("sans titre","")
                                                    [a]   -> return (a,"")
                                                    [a,b] -> return (a,b)

  
  return $ do
    (t,st) <- titles
    return $ Post
      t
      st
      preview
      cover
      body
      []
      doc'
      "NO format"


---- Pandoc
extractHeaders = Doc.query f
  where f :: Block -> [String]
        f (Header _ _ x) = [stringify x]
        f _ = []

extractImages = Doc.query f
  where f :: Inline -> [Image]
        f (Image alt (url,_)) = [(url, stringify alt)]
        f _ = []

extractBody doc = evalState (Doc.walkM f doc) 0
  where f :: Block -> State Int Block
        f h@(Header _ _ _) = do
          c <- get
          if c < 2
            then (put $ c + 1) >> return Null
            else return h
        f x = return x

extractPreview doc = case runState (Doc.walkM f doc) False of
  (p,True)  -> Just p
  (_,False) -> Nothing
  
  where f :: Block -> State Bool Block
        f HorizontalRule = put True >> return Null
        f x = do
          s <- get
          return $ if s then Null else x
  

---- HTML Templates
postFormLogin p i pub last = do
  H.form ! action "" ! A.method "POST" ! enctype "multipart/form-data" $ do
    loginAgain
    postForm' p

  postPreviewHtml p i pub last False
  postHtml p pub last False

postForm p i pub last = do
  H.form ! action "" ! A.method "POST" ! enctype "multipart/form-data" $ postForm' p
  postPreviewHtml p i pub last False
  postHtml p pub last False

postForm' p = do
  H.label ! for "in_content" $ "article"
  textarea ! A.name "content" ! A.id "in_content" $ toHtml $ postSource p

  H.label ! for "in_format" $ "format"
  select ! A.name "format" ! A.id "in_format" $ do
    option "un certain format" -- en attente de Pandoc

  input ! type_ "submit" ! A.name "draft"   ! value "Sauvegarder en brouillon"
  input ! type_ "submit" ! A.name "publish" ! value "Publier"
    

next ref = H.div ! class_ "next" $ do
  a ! href (toValue $ ref) $ "articles suivants"
  H.div ! class_ "thread deco" $ ""

prev ref = H.div ! class_ "prev" $ do
  H.div ! class_ "thread deco" $ ""
  a ! href (toValue $ ref) $ "articles précédents"


dateHtml pub last = let lastHtml = last <$< \(u,d) -> do "modifié " ; (time' u) $ toHtml d
                        pubHtml  = pub  <$< \(u,d) -> H.em $ do "publié " ; (time' u) ! pubdate "" $ toHtml d

                    in if last == Nothing && pub == Nothing then return () else do
                      H.div ! class_ "date" $ do
                        mToHtml pubHtml
                        br
                        mToHtml lastHtml

  
  
    
postHtml :: Post -> (Maybe (UTCTime,String)) -> (Maybe (UTCTime,String)) -> Bool -> Html
postHtml p pub last edit = article ! class_ "post" $ do
  h1 $ a ! href (toValue $ "/blog/post/" ++ (show i)) $ toHtml $ postTitle p
  h2 $ toHtml $ postSubTitle p
  dateHtml pub last
  
  case (postTags p) of [] -> return ()
                       ts -> ul ! class_ "tags" $ forM_ ts (li . toHtml)


  if edit then aa ! href (toValue $ "/blog/edit/" ++ (show i)) $ "modifier" else return ()
  
  H.div ! class_ "body" $ preEscapedToHtml $ postBody p
  


postPreviewHtml :: Post -> Integer -> (Maybe (UTCTime,String)) -> (Maybe (UTCTime,String)) -> Bool -> Html
postPreviewHtml p i pub last edit = article ! class_ "post preview" ! A.id (toValue i) $ do
  H.div ! class_ "header" $ do
    H.div ! class_ "thread deco" $ ""
    dateHtml pub last

    case (postTags p) of [] -> return ()
                         ts -> ul ! class_ "tags" $ forM_ ts (li . toHtml)
                       
  H.div ! class_ "box" $ do
    H.div ! class_ "mask" $ do
      H.div ! class_ "content"   $ do
        h1 $ a ! href (toValue $ "/blog/post/" ++ (show i)) $ toHtml $ postTitle p
        h2 $ toHtml $ postSubTitle p

        if edit then aa ! href (toValue $ "/blog/edit/" ++ (show i)) $ "modifier" else return ()
  
        H.div ! class_ "body" $ preEscapedToHtml $ case (postPreview p) of (Just a) -> a
                                                                           Nothing  -> (postBody p)
      
      aside ! class_ "images" $ forM_ (postCover p) $ \(s,a) -> img
                                                                ! src (toValue s)
                                                                ! alt (toValue a)
                                                                ! A.title (toValue a)

      case (postPreview p) of (Just _) -> a
                                          ! class_ "read-more"
                                          ! href (toValue $ "/blog/post/" ++ (show i)) $
                                          let dot = (H.span "") in dot >> dot >> dot >> "lire la suite"
                                          
                              Nothing  -> return ()
  
  


