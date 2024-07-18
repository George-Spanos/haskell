module CryptoChannels (run) where

import Control.Concurrent
  ( Chan,
    forkIO,
    newChan,
    readChan,
    writeChan,
  )
import Control.Monad (forM_, forever, replicateM_)
import Data.ByteString.Lazy as BS (readFile)
import Data.Digest.Pure.MD5
import System.Environment (getArgs)

nrWorkers :: Int
nrWorkers = 10

run :: IO ()
run = do
  files <- getArgs
  str <- newChan
  fileChan <- newChan
  forM_ [1 .. nrWorkers] (\_ -> forkIO $ worker str fileChan)
  forM_ files (writeChan fileChan)
  printNrResults (Prelude.length files) str

printNrResults :: Int -> Chan String -> IO ()
printNrResults i var = replicateM_ i (readChan var >>= putStrLn)

worker :: Chan String -> Chan String -> IO ()
worker str fileChan = forever (readChan fileChan >>= hashAndPrint str)

hashAndPrint :: Chan [Char] -> FilePath -> IO ()
hashAndPrint str f = do
  bs <- BS.readFile f
  let h = show $ md5 bs
  writeChan str (f ++ ": " ++ h)