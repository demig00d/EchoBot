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
import           Utils                  (prettyShow)


startBot :: FilePath -> IO ()
startBot path = do
  config <- getConfig path
  case config of
    Left message -> logError message

    Right cfg@Config{cPlatformName="telegram",..} ->
      logInfo cLogLevel "Configuration parsed successfully."
      >> logDebug cLogLevel (prettyShow cfg)
      >> logInfo cLogLevel "Telegram platform has been selected."
      >> logInfo cLogLevel "Check request environment and try to get Model from Config."
      >> Telegram.getModel cfg
      >>= either logError
           (\model -> logInfo cLogLevel "Model has been obtained."
                   >> logDebug cLogLevel (prettyShow model)
                   >> runReaderT mainLoop model)

    Right cfg@Config{cPlatformName="vkontakte",..} ->
      logInfo cLogLevel "Configuration parsed successfully."
      >> logDebug cLogLevel (prettyShow cfg)
      >> logInfo cLogLevel "VKontakte platform has been selected."
      >> logInfo cLogLevel "Check request environment and try to get Model from Config."
      >> VKontakte.getModel cfg
      >>= either logError
           (\model -> logInfo cLogLevel "Model has been obtained."
                   >> logDebug cLogLevel (prettyShow model)
                   >> runReaderT mainLoop model)


mainLoop :: (Bot env, MonadReader (Model env) m, MonadIO m) => m ()
mainLoop = do
  model@Model{..} <- ask

  u <- liftIO $ getUpdates mPlatformEnv

  case u of
    Left msg      -> liftIO $ logError msg
    Right updates -> do
      liftIO $ logInfo mLogLevel "Bot got update."

      model' <- liftIO $ handleUpdates model updates
      runReaderT mainLoop model'
