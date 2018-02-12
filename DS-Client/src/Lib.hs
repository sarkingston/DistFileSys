{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib where


import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import Data.Text
import Database.Persist

data File = File
  { datum        :: String
  , name         :: String
  } deriving (Eq, Read, Show, Generic)

instance FromJSON File
instance ToJSON File

data ReplicationServer = ReplicationServer
  {   primary :: Bool
    , ip :: Text

  } deriving (Eq, Read, Show, Generic)

instance FromJSON ReplicationServer
instance ToJSON ReplicationServer


data Server' = Server'
  {
       primaryIP :: Text
    ,  primaryPort :: Int
  } deriving (Eq, Read, Show, Generic)

instance FromJSON Server'
instance ToJSON Server'

data Filelocation = Filelocation
  {
      filename :: Text
    , server' :: Server'
  } deriving (Eq, Read, Show, Generic)

instance FromJSON Filelocation
instance ToJSON Filelocation




type FSApi = "filepush" :> ReqBody '[JSON] File :> Post '[JSON] (Maybe Bool)
  :<|> "filepull" :> QueryParam "filename" String :> Get '[JSON] (Maybe File)


type DSApi =
       "file" :> "add" :> Capture "name" Text :> Post '[JSON] (Maybe Server')
  :<|> "file" :> "get" :> Capture "name" Text  :> Get  '[JSON] (Maybe Filelocation)
  :<|> "makeMePrimary" :> Capture "oldip" Text :> Capture "oldport" Int :> Capture "newip" Text :> Capture "newport" Int :> Get '[JSON] ()
  :<|> "addMeToGroup" :> ReqBody '[JSON] Server' :> Get '[JSON] Server'
  :<|> "createGroup" :> ReqBody '[JSON] Server' :> Get '[JSON] Bool


apiDS :: Proxy DSApi
apiDS = Proxy

fileadd :: Text -> ClientM (Maybe Server')

fileget :: Text -> ClientM (Maybe Filelocation)

makeMePrimary :: Text -> Int -> Text -> Int -> ClientM ()

addMeToGroup :: Server' -> ClientM Server'

createGroup :: Server' -> ClientM Bool

(fileadd :<|> fileget :<|> makeMePrimary :<|> addMeToGroup :<|> createGroup) = client apiDS

apiFS :: Proxy FSApi
apiFS = Proxy

filepush :: File -> ClientM (Maybe Bool)

filepull :: Maybe String -> ClientM (Maybe File)

(filepush :<|> filepull) = client apiFS

queriesFS :: ClientM (Maybe Bool, Maybe Bool, Maybe File)
queriesFS = do
  a0 <- filepush (File "Contents1" "name1")
  a1 <- filepush (File "Contents2" "name2")
  a2 <- filepull (Just "name1")
  return (a0, a1, a2)

queriesDS :: ClientM (Maybe Server', Maybe Server', Maybe Filelocation, Maybe Filelocation)
queriesDS = do
  fa0 <- fileadd "myfile :)"
  fa1 <- fileadd "your file >:("
  fg0 <- fileget "myfile :)"
  fg1 <- fileget "your file >:("
  return (fa0, fa1, fg0, fg1)

-- uploadFile
-- downloadFile
-- reuploadFile

type FileName = String
type IP = String
type Port = Int


--downloads a file to a filepath and locks file
downloadFile :: FileName -> FilePath -> IO ()
downloadFile fname fp = do
  manager <- newManager defaultManagerSettings
  fileloc <- runClientM (fileget (pack fname)) (ClientEnv manager (BaseUrl Http "localhost" 3003 ""))
  case fileloc of
    Left e -> putStrLn $ "file does not exist. Did you spell the name correctly?\n" ++ show e
    Right Nothing -> putStrLn "error in downloads file case, recieved non file object"
    Right (Just (Filelocation name clus)) -> do
      file <- pullfile (unpack name) clus
      case file of
        Just (File datum name) -> writeFile (fp ++ "/" ++ name) datum
        Nothing -> putStrLn "File locked. Try again later."

reuploadFile :: File -> IO ()
reuploadFile f = uf f
--just a temporary measure

pullfile :: FileName -> Server' -> IO (Maybe File)
pullfile name server' = do
  manager <- newManager defaultManagerSettings
  f <- runClientM (filepull (Just name)) (ClientEnv manager (BaseUrl Http (unpack (primaryIP server')) (primaryPort server') ""))
  case f of

    Left e -> do
      putStrLn $ "problem in pullfile Client: " ++ show e
      return Nothing
    Right r -> return r

uf :: File -> IO ()
uf f@(File datum name) = do
  manager <- newManager defaultManagerSettings
  server' <- runClientM (fileadd (pack name)) (ClientEnv manager (BaseUrl Http "localhost" 3003 ""))
  case server' of
    Left e -> putStrLn $ "Error in uf: " ++ show e
    Right (Just c) -> do
      q <- pushfile c f
      if q then putStrLn $ show f ++ " pushed successfuly"
        else putStrLn "push unsuccessful, try again later"


-- This takes a Server' and a file and uploads the file to that server'
pushfile :: Server' -> File -> IO Bool
pushfile (Server' ip port) f = do
  manager <- newManager defaultManagerSettings
  push_res <- runClientM (filepush f) (ClientEnv manager (BaseUrl Http (unpack ip) port ""))
  case push_res of
    Left e -> do
      putStrLn $ "Error in pushfile: " ++ show e
      return False
    Right (Just b) -> return b
    Right Nothing -> do
      putStrLn "Error in the pushfile function"
      return False

runFS :: IO ()
runFS = do
  manager <- newManager defaultManagerSettings
  res <-  runClientM queriesFS (ClientEnv manager (BaseUrl Http "localhost" 3002 ""))
  case res of
    Left e -> putStrLn $ "Error: " ++ show e
    Right (b0, b1, f) -> do
      print b0
      print b1
      print f

runDS :: IO ()
runDS = do
  manager <- newManager defaultManagerSettings
  res <- runClientM queriesDS (ClientEnv manager (BaseUrl Http "localhost" 3003 ""))
  case res of
    Left e -> putStrLn $ "Error: " ++ show e
    Right (c, d, e, f) -> do
      print c
      print d
      print e
      print f
