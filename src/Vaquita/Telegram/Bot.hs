{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Vaquita.Telegram.Bot (startServer) where

import Universum
import System.Environment (getEnv)
import Web.Scotty as S hiding (readEither)
import Network.HTTP.Req as R
import Data.Aeson
import Network.HTTP.Client.TLS
import Network.Connection
import Control.Exception.Safe (throwString)

startServer :: IO ()
startServer = do
  port   <- (either (throwString . toString) pure . readEither) =<< getEnv "PORT"
  token  <- ("bot" <>) . toText <$> getEnv "TOKEN"
  secret <- getEnv "SECRET"
  httpConfig <- mkHttpConfig
  scotty port $
    post (literal $ "/" ++ secret ++ "/updates") $ do
      update <- jsonData
      putTextLn "Processing Update"
      sendMessage httpConfig token update
      S.text "All Good"

mkHttpConfig :: IO HttpConfig
mkHttpConfig = do
  let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
  mgr <- liftIO $ newTlsManagerWith settings
  pure defaultHttpConfig { httpConfigAltManager = Just mgr }

sendMessage :: (MonadIO m) => HttpConfig -> Text -> Update -> m ()
sendMessage httpConfig token (Update (Message (Chat chatId) txt)) = do
  let msg = SendMessage chatId txt
      r   = req POST (https "api.telegram.org" /: token /: "sendMessage") (ReqBodyJson msg) ignoreResponse mempty
  void $ runReq httpConfig r

newtype Update = Update { message :: Message } deriving (Generic)
data Message = Message { chat :: Chat, text :: Text } deriving (Generic)
newtype Chat = Chat { id :: Int } deriving (Generic)
data SendMessage = SendMessage { chat_id :: Int, text :: Text } deriving (Generic)

instance FromJSON Update where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON Message where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON Chat where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON SendMessage where
  toJSON = genericToJSON defaultOptions

