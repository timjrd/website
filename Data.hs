{-# LANGUAGE CPP
            ,DeriveDataTypeable
            ,FlexibleContexts
            ,GeneralizedNewtypeDeriving
            ,StandaloneDeriving
            ,MultiParamTypeClasses
            ,TemplateHaskell
            ,TypeFamilies
            ,RecordWildCards
            #-}

module Data where


import Shared

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

data DataBase = DataBase { projects    :: [Project']
                         , projectsIds :: IdMap
                         , session     :: Session
                         }
              deriving (Data, Typeable)

initialDataBase = DataBase
                  [demoProject']
                  M.empty
                  Closed

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

demoProject' = Project' "uselesoft" (Published demoProject Nothing)

demoProject = Project
              "UseleSoft"
              "logiciel de voyance précognitive"
              desc
              desc
              "brut"
              "projet associatif"
              "un des principaux dévellopeurs"
              ["Haskell","html/css"]
              ["Happstack","BlazeHtml","acid-state"]
              [(img,"boule de crystale")
              ,(img,"boule de crystale")]
              (Just "http://www.nyan.cat/")
              Nothing

  where desc = "UseleSoft est une implémentation numérique de la precrime (Minority Report), le tout étant bien sûr une arnaque (boule de cristale). Kamoulox."
        img = "http://upload.wikimedia.org/wikipedia/commons/9/99/John_William_Waterhouse_-_The_Crystal_Ball.JPG"



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
                           
                           ,'updateSession
                           ,'getSession
                           ,'closeSession
                           ])
