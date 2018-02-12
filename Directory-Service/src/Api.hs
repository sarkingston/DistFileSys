{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import Data.Proxy
import Data.Text

import Database.Persist

import Models

import Servant.API



type DSApi =
       "file" :> "add" :> Capture "name" Text :> Post '[JSON] (Maybe Server')
  :<|> "file" :> "get" :> Capture "name" Text :> Get  '[JSON] (Maybe Filelocation)
  :<|> "makemeprimary" :> Capture "oldip" String :> Capture "oldport" Int :> Capture "newip" String :> Capture "newport" Int :> Get '[JSON] ()
  :<|> "addmetogroup" :>  Capture "ip" String :> Capture "port" Int :> Get '[JSON] (Maybe Server')
  :<|> "creategroup" :> Capture "ip" String :> Capture "port" Int :> Get '[JSON] Bool
-- ReqBody '[JSON] Server'
--  :<|> "addcluster" :> Capture "ip" Text :> Capture "port" Int :> Get '[JSON] Bool

api :: Proxy DSApi
api = Proxy
