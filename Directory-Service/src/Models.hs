{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import Data.Aeson
import Data.Text

import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Server'
  primaryIP String
  primaryPort Int
  deriving Eq Read Show
Groups
  primary Server'
  size Int
  deriving Eq Read Show
Filelocation
  filename Text
  server'  Server'
  isLocked Bool
  deriving Eq Read Show
|]

instance FromJSON Filelocation where
  parseJSON = withObject "Filelocation" $ \ v ->
    Filelocation <$> v .: "filename"
         <*> v .: "server'"
         <*> v .: "isLocked"

instance ToJSON Filelocation where
  toJSON (Filelocation filename server' isLocked) =
    object [ "filename" .= filename
            , "server'" .= server'
            , "isLocked" .= isLocked ]

instance FromJSON Server' where
    parseJSON = withObject "Server'" $ \ v ->
      Server' <$> v .: "primaryIP"
        <*> v .: "primaryPort"


instance ToJSON Server' where
  toJSON (Server' primaryIP primaryPort) =
      object [ "primaryIP" .= primaryIP
              , "primaryPort" .= primaryPort]

instance FromJSON Groups where
  parseJSON = withObject "Groups" $ \ v ->
    Groups <$> v .: "primary"
        <*> v .: "size"


instance ToJSON Groups where
  toJSON (Groups primary size) =
      object [ "primary" .= primary
          , "size" .= size]
