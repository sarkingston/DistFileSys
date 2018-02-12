{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Api where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Proxy
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Client
import System.Directory
import Data.Text
import qualified Data.Aeson.Parser
import Models
-- import Server'
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API

-- filepush is where the body of the request is a json file with a file in it in a bytestring, it returns Just the name of the file that has been successfully uploaded or nothing.
type FSApi = "filepush" :> ReqBody '[JSON] File :> Post '[JSON] (Maybe Bool)
  :<|> "filepull" :> QueryParam "filename" String :> Get '[JSON] (Maybe File)
  :<|> "beagroup" :> Capture "ip" String :> Capture "port" Int :> Get '[JSON] Bool
  :<|> "joinagroup" :> Capture "ip" String :> Capture "port" Int :> Get '[JSON] ()
  :<|> "letmejoin" :> Capture "ip" String :> Capture "port" Int :> Get '[JSON] [Server']

api :: Proxy FSApi
api = Proxy

filepush :: File -> ClientM (Maybe Bool)

filepull :: Maybe String -> ClientM (Maybe File)

beagroup :: String -> Int -> ClientM Bool

joinagroup :: String -> Int -> ClientM ()

letmejoin :: String -> Int -> ClientM [Server']


(filepush :<|> filepull :<|> beagroup :<|> joinagroup :<|> letmejoin) = client api
--
data Groups = Groups
  { primary :: Server'
  , size :: Int
  } deriving (Eq, Read, Show, Generic)

instance FromJSON Groups
instance ToJSON Groups

data Filelocation = Filelocation
  {  filename :: Text
  ,  server' :: Server'
  ,  isLocked :: Bool

  } deriving (Eq, Read, Show, Generic)


instance FromJSON Filelocation
instance ToJSON Filelocation

type DSApi =
     "file" :> "add" :> Capture "name" Text :> Post '[JSON] (Maybe Server')
  :<|> "file" :> "get" :> Capture "name" Text  :> Get  '[JSON] (Maybe Filelocation)
  :<|> "makemeprimary" :> Capture "oldip" String :> Capture "oldport" Int :> Capture "newip" String :> Capture "newport" Int :> Get '[JSON] ()
  :<|> "addmetogroup" :>  Capture "ip" String :> Capture "port" Int :> Get '[JSON] (Maybe Server')
  :<|> "creategroup" :> Capture "ip" String :> Capture "port" Int :> Get '[JSON] Bool


apiDS :: Proxy DSApi
apiDS = Proxy

fileadd :: Text -> ClientM (Maybe Server')

fileget :: Text -> ClientM (Maybe Filelocation)

makemeprimary :: String -> Int -> String -> Int -> ClientM ()

addmetogroup :: String -> Int -> ClientM (Maybe Server')

creategroup :: String -> Int -> ClientM Bool

(fileadd :<|> fileget :<|> makemeprimary :<|> addmetogroup :<|> creategroup) = client apiDS
