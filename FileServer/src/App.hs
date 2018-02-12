{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module App where

import           Control.Monad.IO.Class
import           Control.Monad.Logger (runStderrLoggingT)
import Control.Exception
import Control.Monad
import Data.Maybe
import           Data.String.Conversions

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite

import           Network.Wai
import           Network.Wai.Handler.Warp as Warp

import           Servant
import System.IO
-- import Server'
import Servant.Client
import Network.HTTP.Client (newManager, defaultManagerSettings)
import           Data.Text

import           Api
import           Models

askForGroup :: Server' -> IO (Maybe Server')
askForGroup s1@(Server' oip oport) = do
  manager <- newManager defaultManagerSettings
  x <- runClientM (addmetogroup oip oport) (ClientEnv manager (BaseUrl Http "localhost" 3003 ""))
  case x of
    Left e -> do
      putStrLn $ "Error is askForGroup in FileServer: " ++ show e
      return Nothing
    Right s -> return s


notifyDS :: String -> Int -> IO Bool
notifyDS ip port = do
  manager <- newManager defaultManagerSettings
  x <- runClientM (creategroup ip port) (ClientEnv manager (BaseUrl Http "localhost" 3003 ""))
  case x of
    Left e -> do
      putStrLn $ "Error in notifyDs in Fileserver: " ++ show e
      return False
    Right x -> return True

askToJoin :: Server' -> String -> Int -> IO ( Maybe [Server'])
askToJoin s@(Server' sip sport) ip port = do
  manager <- newManager defaultManagerSettings
  x <- runClientM (letmejoin sip sport) (ClientEnv manager (BaseUrl Http ip port ""))
  case x of
    Left e -> do
      return Nothing
    Right s -> return $ Just s

--       putStrLn $ "Error in ask to join in fileserver: " ++ show e

server :: ConnectionPool -> Server FSApi
server pool =
  filePushH :<|> filePullH :<|> makePrimaryOfGroupH :<|>joinAGroupH :<|> createGroupH

  where
    filePullH fname = liftIO $ filePull fname
    filePushH f = liftIO $ filePush f
    makePrimaryOfGroupH ip port = liftIO $ beGroup ip port
    joinAGroupH ip port = liftIO $ joinAGroup ip port
    createGroupH ip port = liftIO $ addMeToGroup ip port

    filePull :: Maybe String -> IO (Maybe File)
    filePull (Just f) = flip runSqlPersistMPool pool $ do
      mFile <- (selectFirst [FileName ==. f] [])
      return $ entityVal <$> mFile
    filePull Nothing = return Nothing


    filePush :: File -> IO (Maybe Bool)
    filePush f = flip runSqlPersistMPool pool $ do
      exists <- selectFirst [FileName ==. (fileName f)] []
      case exists of
        Nothing -> do
          Just <$> insert f
          return $ Just True
        Just _ -> return Nothing

-- be a group :
-- send http request to DS to make a group
-- set Primary table ip and port
-- inclue ones self in the ReplicationServer table
    beGroup :: String -> Int -> IO Bool
    beGroup ip port = flip runSqlPersistMPool pool $ do
      insert (Primary ip port)
      insert ((Server' ip port))
      insert (ReplicationServer (Server' ip port))
      res <- liftIO $ notifyDS ip port
      return res


-- join a group:
-- ask DS for a group to join
-- send join message to FileServer specified by DS
-- recieve list of servers in group
-- add main server as your primary
-- add other servers to replication list
    joinAGroup ::String -> Int -> IO ()
    joinAGroup ip port = flip runSqlPersistMPool pool $ do
      prime <- liftIO $ askForGroup (Server' ip port)
      case prime of
        Nothing -> return ()
        Just y -> do
          res <- insert (Primary ip port)
          s <- liftIO $ askToJoin (Server' ip port) (server'PrimaryIP y) (server'PrimaryPort y)
          case s of
            Nothing -> return ()
            Just x -> do
              mapM  insert x
              return ()


-- add server to ReplicationServer
    addMeToGroup :: String -> Int -> IO ([Server'])
    addMeToGroup ip port = flip runSqlPersistMPool pool $ do
      x <- insert (Server' ip port)
      ls <- selectList [Server'PrimaryIP !=. ip] []
      return $ entityVal <$> ls






app :: ConnectionPool -> Application
app pool = serve api $ server pool

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
    pool <- runStderrLoggingT $ do
      createSqlitePool (cs sqliteFile) 5

    runSqlPool (runMigration migrateAll) pool
    return $ app pool

run :: FilePath  -> String -> IO ()
run sqliteFile port = do
    Warp.run (read port) =<< mkApp sqliteFile
