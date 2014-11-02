{-# LANGUAGE CPP
            ,DeriveDataTypeable
            ,FlexibleContexts
            ,GeneralizedNewtypeDeriving
            ,StandaloneDeriving
            ,MultiParamTypeClasses
            ,TemplateHaskell
            ,TypeFamilies
            ,RecordWildCards
            ,QuasiQuotes
            #-}

module Data where


import Shared

import Text.RawString.QQ

import Control.Applicative  ( (<$>) )
import Control.Exception    ( bracket )
import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get, put )
import Data.Data            ( Data, Typeable )
import qualified Data.List as L

import Data.Acid            ( AcidState, Query, Update
                            , makeAcidic, openLocalState )
import Data.Acid.Advanced   ( query', update' )
import Data.Acid.Local      ( createCheckpointAndClose )
import Data.SafeCopy        ( deriveSafeCopy )
import qualified Data.SafeCopy as C

import Data.ByteString (ByteString)
import Data.Time
import qualified Data.Map as M
import Data.IxSet



data DataBase = DataBase { session     :: Session

                         , projects    :: [Project']
                         , projectsIds :: IdMap

                         , blog        :: IxSet PublishablePost
                         , nextPostId  :: Integer
                         }
              deriving (Data, Typeable)

initialDataBase = DataBase
                  Closed
                  
                  []
                  M.empty

                  empty
                  1

---- Session
data Session = Session ByteString UTCTime | Closed
             deriving (Data, Typeable)

closeSession :: Update DataBase ()
closeSession = do
  db <- get
  put $ (db {session = Closed})


updateSession :: Session -> Update DataBase ()
updateSession s = do
  db <- get
  put $ (db {session = s})

getSession :: Query DataBase Session
getSession = session <$> ask



---- Blog
                        
data PublishablePost = PublishedPost { postId        :: Integer
                                     , postDate      :: UTCTime         -- publication
                                     , postLast      :: (Maybe UTCTime) -- dernière modification
                                     , publishedPost :: Post            -- article publié
                                     , postDraft     :: (Maybe Post)    -- brouillon
                                     }

                     | PostDraft Integer Post
                       
                     deriving (Eq, Ord, Data, Typeable)

data Post = Post { postTitle    :: String
                 , postSubTitle :: String
                 , postPreview  :: Maybe String
                 , postCover    :: [Image]
                 , postBody     :: String
                 , postTags     :: [String]
                 , postSource   :: String
                 , postFormat   :: String
                 }
          deriving (Eq, Ord, Data, Typeable)

data PostStatus = PostStatusDraft | PostStatusPublished
          deriving (Eq, Ord, Data, Typeable)

instance Indexable PublishablePost where
  empty = ixSet [ ixFun $ \bp -> case bp of (PublishedPost i _ _ _ _) -> [i] --- id
                                            (PostDraft     i _)       -> [i]
                                            
                , ixFun $ \bp -> case bp of (PublishedPost _ _ _ _ _) -> [PostStatusPublished] --- Status
                                            (PostDraft     _ _)       -> [PostStatusDraft]
                                            
                , ixFun $ \bp -> case bp of (PublishedPost _ d _ _ _) -> [d] --- date publication
                                            (PostDraft     _ _)       -> []

                , ixFun $ \bp -> case bp of (PublishedPost _ _ _ p _) -> postTags p --- tags
                                            (PostDraft     _ _)       -> []

                ]



demoPost = [r|
*  Titre
** sous-titre

- une
- list
- de tags

----------------------

Ici l'introduction avant coupure.

----------------------

Et la suite ici...
|]

-- demoPost' = PublishedPost
--             0
--             (UTCTime (fromGregorian 2012 12 31) 0)
--             (Just $ UTCTime (fromGregorian 2013 01 01) 0)
--             demoPost
--             Nothing

-- demoPrev = "Aujourd'hui quelqu'un est mort. Né dans un petit village, il n'est plus."
-- demoBod  = demoPrev ++ "Il est sous terre, peut être se nourrit-il de pissenlits."

-- demoPost = Post
--            "Il est re-vivant !"
--            "oui c'est terrible, mais c'est la vie"
--            (Just demoPrev)
--            [ (demoImg,"bsod")
--            , (demoImg',"bsod") ]
--            demoBod
--            ["Informatique", "Haskell", "Programmation"]
--            demoBod
--            "kamoulox"

blogPage :: Int -> Query DataBase ([PublishablePost], Bool)
blogPage np = do
  all <- blog <$> ask
  let n = np - 1
      ps = drop (n*7) $ toDescList (Proxy :: Proxy UTCTime) (all @= PostStatusPublished)
      (as,bs) = splitAt 7 $ ps
      
  return $ if length bs <= 4
           then (ps,False)
           else (as,True)

lastsPosts :: Int -> Query DataBase [PublishablePost]
lastsPosts n = do
  ps <- blog <$> ask
  return $
    take n $ toDescList (Proxy :: Proxy UTCTime) (ps @= PostStatusPublished)
  
postsBefore :: Int -> UTCTime -> Query DataBase [PublishablePost]
postsBefore n date = do
  ps <- blog <$> ask
  return $
    take n $ toDescList (Proxy :: Proxy UTCTime) (ps @= PostStatusPublished @< date)

postsAfter :: Int -> UTCTime -> Query DataBase [PublishablePost]
postsAfter n date = do
  ps <- blog <$> ask
  return $
    reverse $ take n $ toAscList (Proxy :: Proxy UTCTime) (ps @= PostStatusPublished @> date)

getPost :: Integer -> Bool -> Query DataBase (Maybe PublishablePost)
getPost i draft = do
  ps <- blog <$> ask
  return $ getOne $ (if draft then ps else ps @= PostStatusPublished) @= i

getPostDrafts :: Query DataBase [PublishablePost]
getPostDrafts = do
  ps <- blog <$> ask
  return $ toDescList (Proxy :: Proxy Integer) (ps @= PostStatusDraft)

draftNewPost :: Post -> Update DataBase Integer
draftNewPost p = do
  db <- get
  let i = nextPostId db
  put $ db { blog = insert (PostDraft i p) (blog db)
           , nextPostId = i + 1
           }
  return i

publishNewPost :: Post -> UTCTime -> Update DataBase Integer
publishNewPost p ct = do
  db <- get
  let i = nextPostId db
  put $ db { blog = insert (PublishedPost i ct Nothing p Nothing) (blog db)
           , nextPostId = i + 1
           }
  return i


publishPost :: Integer -> Post -> UTCTime -> Update DataBase Integer
publishPost i p ct = do
  db <- get
  let (Just old) = getOne $ (blog db) @= i
      new = case old of (PublishedPost _ pub _ _ _) -> PublishedPost i pub (Just ct) p Nothing
                        (PostDraft     _ _)         -> PublishedPost i ct Nothing p Nothing
      
  put $ db { blog = updateIx i new (blog db)
           }
  return i

draftPost :: Integer -> Post -> Update DataBase Integer
draftPost i p = do
  db <- get
  let (Just old) = getOne $ (blog db) @= i
      new = case old of o@(PublishedPost _ _ _ _ _) -> o { postDraft = Just p }
                        (PostDraft       _ _)       -> PostDraft i p
      
  put $ db { blog = updateIx i new (blog db)
           }
  return i

unpublishPost :: Integer -> Update DataBase ()
unpublishPost i = do
  db <- get
  let (Just old) = getOne $ (blog db) @= i
      new = case old of (PublishedPost _ _ _ p Nothing)  -> PostDraft i p
                        (PublishedPost _ _ _ _ (Just d)) -> PostDraft i d
                        a -> a
      
  put $ db { blog = updateIx i new (blog db)
           }



---- Code (Projects)
type IdMap = M.Map String Int
type Id = String

data Publishable a = Published a (Maybe a) | Draft a
                   deriving (Data, Typeable)

data Project' = Project' { projectId :: Id
                         , project   :: Publishable Project
                         }
              deriving (Data, Typeable)
                
  
data Project = Project { projectName  :: String
                        ,kind         :: String
                        ,desc         :: String
                        ,source       :: String
                        ,sourceFormat :: String
                        ,context      :: String
                        ,role         :: String
                        ,mainTechs    :: [String]
                        ,otherTechs   :: [String]
                        ,images       :: [Image]
                        ,moreDetails  :: Maybe Url
                        ,download     :: Maybe Url
                        }
             deriving (Data, Typeable)

type Url = String
type Image = (Url,String)

demoProject = [r|
*  Nom du projet
** sous-titre

- cadre du projet
- rôle personel


- principales
- technologies
- utilisés


- technologies
- secondaires


- http://url-projet
- http://url-telechargement

-----------------------

Description du projet...
|]

-- demoProject' = Project' "uselesoft" (Published demoProject Nothing)

-- demoProject = Project
--               "UseleSoft"
--               "logiciel de voyance précognitive"
--               desc
--               desc
--               "brut"
--               "projet associatif"
--               "un des principaux dévellopeurs"
--               ["Haskell","html/css"]
--               ["Happstack","BlazeHtml","acid-state"]
--               [(demoImg,"bsod")
--               ,(demoImg',"bsod")]
--               (Just "http://www.nyan.cat/")
--               Nothing

--   where desc = "UseleSoft est une implémentation numérique de la precrime (Minority Report), le tout étant bien sûr une arnaque (boule de cristale). Kamoulox."
-- demoImg = "http://www.pc-code.com/base/numetlet/let/b/images/bsod.jpg"
-- demoImg' = "http://upload.wikimedia.org/wikipedia/commons/a/ae/Windows8-BSOD.jpg"


insertAt i x xs = let (a,b) = splitAt i xs in a ++ [x] ++ b
deleteAt i xs = let (a,b) = splitAt i xs in a ++ (tail b)

modifyProjects f = do
  db <- get
  put $ (db {projects = f (projects db)})

modifyProjectAt i f = modifyProjects $ \ps -> 
  let (as, ((Project' _ old):bs)) = break ((i==) . projectId) ps
  in as ++ [Project' i (f old)] ++ bs

makeId orig = do
  db <- get
  let (count,newmap) = M.insertLookupWithKey (\_ _ old -> old+1) orig 2 (projectsIds db)
  put $ (db {projectsIds = newmap})
  case count of Nothing  -> return $ orig
                Just new -> return $ orig ++ " (" ++ show new ++ ")"
      

publishNewProject :: Int -> Project -> Update DataBase Id
publishNewProject pos x = do
  i <- makeId (projectName x)
  let p = Project' i (Published x Nothing)
  modifyProjects $ insertAt pos p
  return i

draftNewProject :: Int -> Project -> Update DataBase Id
draftNewProject pos x = do
  i <- makeId (projectName x)
  let p = Project' i (Draft x)
  modifyProjects $ insertAt pos p
  return i

publishProject :: Id -> Maybe Int -> Project -> Update DataBase Id
publishProject i pos new = do
  db <- get
  let (as,(Project' _ old):bs) = break ((i==) . projectId) (projects db)

  newi <- case old of (Published _ _) -> return i
                      (Draft _  )     -> makeId (projectName new)

  let new' = Project' newi (Published new Nothing)
      newlist = case pos of Nothing -> as++[new']++bs
                            Just p  -> insertAt p new' (as++bs)
  
  put $ (db {projects = newlist})
  return newi

draftProject :: Id -> Maybe Int -> Project -> Update DataBase Id
draftProject i pos new = do
  db <- get
  let (as,(Project' _ old):bs) = break ((i==) . projectId) (projects db)

      new' = case old of (Published a _) -> Project' i $ Published a (Just new)
                         (Draft _)       -> Project' i $ Draft new

      newlist = case pos of Nothing -> as++[new']++bs
                            Just p  -> insertAt p new' (as++bs)
  
  put $ (db {projects = newlist})
  return i


unpublishProject :: Id -> Update DataBase ()
unpublishProject i = modifyProjectAt i $ \p ->
  case p of (Published _ (Just a))  -> Draft a
            (Published a Nothing)   -> Draft a
            _ -> p





deleteProject :: Id -> Update DataBase ()
deleteProject i = modifyProjects $ filter ((i/=) . projectId)


allProjects :: Query DataBase [Project']
allProjects = projects <$> ask

publishedProjects :: Query DataBase [Project']
publishedProjects = filter f . projects <$> ask
  where f (Project' _ (Published p _)) = True
        f (Project' _ (Draft _))       = False

projectIds :: Query DataBase [Id]
projectIds = map (\(Project' i _) -> i) . projects <$> ask
  

findProject :: Id -> Query DataBase (Maybe Project')
findProject i = L.find ((i==) . projectId) . projects <$> ask

editProject :: Id -> Query DataBase (Maybe Project)
editProject i = do
  mp <- L.find ((i==) . projectId) . projects <$> ask
  return $ edit <$> mp

  where edit pr = case (project pr) of (Draft d) -> d
                                       (Published d Nothing)  -> d
                                       (Published _ (Just d)) -> d



---- acid-states templates
$(deriveSafeCopy 0 'C.base ''Session)

$(deriveSafeCopy 0 'C.base ''Post)
$(deriveSafeCopy 0 'C.base ''PublishablePost)
  
$(deriveSafeCopy 0 'C.base ''Project)
$(deriveSafeCopy 0 'C.base ''Publishable)
$(deriveSafeCopy 0 'C.base ''Project')

$(deriveSafeCopy 0 'C.base ''DataBase)
  
$(makeAcidic ''DataBase [ 'publishNewProject
                          , 'draftNewProject
                          , 'publishProject
                          , 'draftProject
                          , 'unpublishProject
                          , 'deleteProject
                          , 'allProjects
                          , 'publishedProjects
                          , 'projectIds
                            , 'findProject
                          , 'editProject

                            , 'blogPage
                            , 'lastsPosts
                              , 'postsBefore
                              , 'postsAfter
                          , 'getPost
                            , 'getPostDrafts
                          , 'draftNewPost
                            , 'publishNewPost
                          , 'publishPost
                            , 'draftPost
                          , 'unpublishPost
                           
                           ,'updateSession
                           ,'getSession
                           ,'closeSession
                           ])
