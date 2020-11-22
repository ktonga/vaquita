{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Vaquita.Telegram.Bot (startServer) where

import System.Environment (getEnv)
import Web.Scotty as S
import Network.HTTP.Req as R
import Data.Aeson
import Data.Text (Text, pack)
import Control.Monad.IO.Class (liftIO)
import GHC.Generics
import Data.Functor (void)
import Network.HTTP.Client.TLS
import Network.Connection

startServer :: IO ()
startServer = do
  port   <- read <$> getEnv "PORT"
  token  <- ("bot" <>) . pack <$> getEnv "TOKEN"
  secret <- getEnv "SECRET"
  httpConfig <- mkHttpConfig
  scotty port $
    post (literal $ "/" ++ secret ++ "/updates") $ do
      update <- S.jsonData
      liftIO $ putStrLn "Processing Update"
      liftIO $ sendMessage httpConfig token update
      S.text "All Good"

mkHttpConfig :: IO HttpConfig
mkHttpConfig = do
  let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
  mgr <- liftIO $ newTlsManagerWith settings
  pure defaultHttpConfig { httpConfigAltManager = Just mgr }

sendMessage :: HttpConfig -> Text -> Update -> IO ()
sendMessage httpConfig token (Update (Message (Chat chatId) txt)) = do
  let msg = SendMessage chatId txt
      r   = req POST (https "api.telegram.org" /: token /: "sendMessage") (ReqBodyJson msg) ignoreResponse mempty
  void $ runReq httpConfig r

newtype Update = Update { message :: Message } deriving (Generic)
data Message = Message { chat :: Chat, text :: String } deriving (Generic)
newtype Chat = Chat { id :: Int } deriving (Generic)
data SendMessage = SendMessage { chat_id :: Int, text :: String } deriving (Generic)

instance FromJSON Update where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON Message where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON Chat where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON SendMessage where
  toJSON = genericToJSON defaultOptions

