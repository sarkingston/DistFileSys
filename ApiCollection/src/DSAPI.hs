{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module DSAPI where

import Data.Aeson
import Data.Aeson.TH
import GHC.Generics
import Data.Proxy
import Data.Text
import Database.Persist
import Servant

data Cluster = Cluster {
  primaryIp :: Text
} deriving (Eq, Read, Show)

data Filelocation = Filelocation {
    filename :: Text
  ,  cluster :: Text
} deriving (Eq, Read, Show)

type DSApi =
       "file" :> "add" :> Capture "name" Text :> Post '[JSON] (Maybe Cluster)
  :<|> "file" :> "get" :> Capture "name" Text  :> Get  '[JSON] (Maybe Filelocation)
  :<|> "addCluster" :> Capture "ip" Text :> Post '[JSON] (Maybe (Key Cluster))
