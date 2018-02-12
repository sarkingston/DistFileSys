{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}

module Models where

import Prelude ()
import Prelude.Compat

import Data.Aeson
import Data.Text
import Data.ByteString
-- import Server'
import Database.Persist.TH
import GHC.Generics


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Server'
  primaryIP String
  primaryPort Int
  deriving Eq Read Show
File
  datum String
  name String
  deriving Eq Read Show
ReplicationServer
  group Server'
  deriving Eq Read Show
Primary
  primaryIP String
  primaryPort Int
  deriving Eq Show Read
|]

 -- primry = main server holding rpimary copies
 -- group = The servers in that group holding backups
 -- Server' bad name, basically just the details of a server (port, ip)

instance FromJSON ReplicationServer where
  parseJSON = withObject "ReplicationServer" $ \ v ->
    ReplicationServer <$> v .: "group"

instance ToJSON ReplicationServer where
  toJSON (ReplicationServer group) =
    object [ "group" .= group ]

instance FromJSON Server' where
  parseJSON = withObject "Server'" $ \ v ->
    Server' <$> v .: "primaryIP"
         <*> v .: "primaryPort"

instance ToJSON Server' where
  toJSON (Server' primaryIP primaryPort) =
    object [ "primaryIP" .= primaryIP
    , "primaryPort" .= primaryPort ]

instance FromJSON File where
  parseJSON = withObject "File" $ \ v ->
    File <$> v .: "datum"
         <*> v .: "name"

instance ToJSON File where
  toJSON (File datum name) =
    object [ "datum" .= datum
    , "name" .= name ]


instance FromJSON Primary where
  parseJSON = withObject "Primary" $ \ v ->
    Primary <$> v .: "primaryIP"
         <*> v .: "primaryPort"

instance ToJSON Primary where
  toJSON (Primary primaryIP primaryPort) =
    object [ "primaryIP" .= primaryIP
    , "primaryPort" .= primaryPort ]
