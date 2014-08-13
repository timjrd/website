
module Main where

import Templates
import Conf
-- import qualified Blog
-- import qualified Code
-- import qualified Portfolio
-- import qualified Photos
-- import qualified CV

import System.Environment (getArgs, getEnv)
import Happstack.Lite
import qualified Happstack.Server as S

main = do
  [host,port] <- getArgs
  datadir <- getEnv "OPENSHIFT_DATA_DIR"
  s <- S.bindIPv4 host (read port)
  S.simpleHTTPWithSocket s (S.nullConf {S.port=(read port)}) $ website (datadir++staticDir)

--website :: ServerPart Response
website d = msum
            [ dir "blog"    $ ok $ toResponse $ page "" "blog" ""
            , dir "code"    $ ok $ toResponse $ page "" "code" ""
            , dir "photos"  $ ok $ toResponse $ page "" "photos" ""
            , dir "cv"      $ ok $ toResponse $ page "Timothée Jourde, CV" "cv" cv
            , dir "static"  $ serveStatic d
            , ok $ toResponse $ page "" "" ""
            ]


serveStatic = serveDirectory EnableBrowsing []
