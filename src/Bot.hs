{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
module Bot (startBot) where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.ByteString.Char8  as S8

import           Bot.Telegram           as Telegram
import           Bot.Types
import           Bot.VKontakte          as VKontakte
import           Configuration
import           Logging


startBot :: FilePath -> IO ()
startBot path = do
  config <- getConfig path
  case config of
    Left message -> logError message

    Right cfg@Config{cPlatformName="telegram",..} ->

      logInfo cLogLevel ("Configuration file has been read and decoded." :: String)
      >> logInfo cLogLevel ("Telegram platform was selected." :: String)
      >> Telegram.getModel cfg  >>= either logError (runReaderT mainLoop)

    Right cfg@Config{cPlatformName="vkontakte",..} ->
      logInfo cLogLevel ("Configuration file has been read and decoded." :: String)
      >> logInfo cLogLevel ("VKontakte platform was selected." :: String)
      >> VKontakte.getModel cfg >>= either logError (runReaderT mainLoop)


mainLoop :: (Bot env, MonadReader (Model env) m, MonadIO m) => m ()
mainLoop = do
  model@Model{..} <- ask
  u <- liftIO $ getUpdates mPlatformEnv
  case u of
    Left msg      -> liftIO $ logError msg
    Right updates -> do
      model' <- liftIO $ handleUpdates model updates
      runReaderT mainLoop model'

