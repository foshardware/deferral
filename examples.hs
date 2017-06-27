{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Deferral
import System.Directory
import System.FilePath
import System.IO


type Swift = DeferralT SomeException

run :: Swift IO a -> IO (Either SomeException a)
run = runDeferralT forkIO

main = do
  a <- run content
  threadDelay 500000
  putStrLn $ show a
  main

-- | Different data sources that are handled differently;
-- After successfully parsing content from a source,
-- defered actions are forked into background
-- and the function returns without trying any more data sources
content :: Swift IO Int
content = do
    hdl <- liftIO $ openFile "content.csv" ReadMode
    defer $ do
      hClose hdl
      removeFile "content.csv"
    liftIO $ parseCSV hdl
  <|> do
    hdl <- liftIO $ openFile "content.xls" ReadMode
    defer $ do
      hClose hdl
      renameFile "content.xls" $ "xls" </> "content.xls"
    liftIO $ parseXLS hdl
  <|> do
    defer $ putStrLn "trying sqlite"
    hdl <- liftIO $ openSQLite "content.sqlite"
    defer $ hClose hdl
    res <- liftIO $ doesFileExist "content"
    guard res
    liftIO $ fromSQLite hdl
  <|> return 0
  where
  parseCSV _ = return 1
  parseXLS _ = return 2
  openSQLite f = openFile f ReadMode
  fromSQLite _ = return 3

nested :: Swift IO ()
nested = do
  defer $ putStrLn "test4"
  liftIO . run $ do
    defer $ putStrLn "test1"
    liftIO $ putStrLn "test0"
  defer $ putStrLn "test3"
  liftIO $ putStrLn "test2"

recursive :: Num a => Swift IO [a]
recursive = mdo
  liftIO $ putStrLn "computing fibs"
  fibs <- pure $ 1 : 1 : zipWith (+) fibs (tail fibs)
  defer $ putStrLn "test"
  return fibs
