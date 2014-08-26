
module Main where

import System.IO
import System.Environment (getEnv, getArgs)
import Crypto.PasswordStore
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)

main = do
  args <- getArgs
  file <- case args of []    -> do d <- getEnv "OPENSHIFT_DATA_DIR"
                                   return $ d ++ "/PASSWORD"
                                   
                       (f:_) -> return f
  
  hSetEcho stdout False
  log "type it> "
  pass1 <- getLine
  log "\nagain> "
  pass2 <- getLine

  if pass1 /= pass2
    then logLn "\npffff try again\n" >> main
         
    else do
    log "\n\ncomputing hash... "
    hash <- makePassword (pack pass1) 1000
    logLn "done."
    logLn "writing it to disk"
    BS.writeFile file hash
    
    log "testing... "
    rhash <- BS.readFile file
    if verifyPassword (pack pass2) rhash
      then logLn "ok."
      else logLn "FAILED !"

  where log x   = hPutStr   stderr x >> hFlush stderr
        logLn x = hPutStrLn stderr x >> hFlush stderr
