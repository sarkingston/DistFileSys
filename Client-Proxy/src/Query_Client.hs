{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Query_Client where

import Data.Aeson
import Data.Proxy
import Data.Text
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import qualified Servant.Client as SC
import Control.Monad.Trans.Either
import AuthApi
import Database.Persist
import Servant

api :: Proxy Auth-Api
api = Proxy

getUser :: SC.ClientM (Maybe User)

-- doPutUser u p = doCall
putUser :: User ->  SC.ClientM (Maybe (Key User))

(getUser :<|> putUser) = SC.client api (BaseUrl Http "localhost" 3000 )

query :: SC.ClientM ((Maybe User), (Maybe (Key User)))
query = do
  gets <- getUser "Alice"
  puts <- putUser "jim" "passs"
  return (gets, puts)

run :: IO ()
run = do
  manager <- newManager defaultManagerSettings
  res <- runClientM query (ClientEnv manager (BaseUrl Http "localhost" 3000 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (gets, puts) -> do
      print gets
      print puts
