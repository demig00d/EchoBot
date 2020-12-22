{-# LANGUAGE FlexibleContexts #-}
module Bot (startBot) where

import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad.Reader

import           Bot.Telegram           as Telegram
import           Bot.Types
import           Bot.VKontakte          as VKontakte
import           Configuration
import           Logging
import           Utils                  (prettyShow, prettyShowMap)


startBot :: FilePath -> IO ()
startBot path = do
  config <- getConfig path
  case config of
    Left message -> logError message

    Right cfg@Config{..} ->
      logInfo cLogLevel "Configuration parsed successfully."
      >> logDebug cLogLevel (prettyShow cfg)
      >> logInfo  cLogLevel "Check request environment and try to get Model from Config."
      >> case cPlatformName of
        "telegram"  -> Telegram.getModel cfg
                    >>= either logError
                         (\model -> logInfo  cLogLevel "Model has been obtained."
                                 >> logDebug cLogLevel (prettyShow model)
                                 >> runReaderT mainLoop model)
        "vkontakte" -> VKontakte.getModel cfg
                    >>= either logError
                         (\model -> logInfo  cLogLevel "Model has been obtained."
                                 >> logDebug cLogLevel (prettyShow model)
                                 >> runReaderT mainLoop model)

        _           ->  logError "Unrecognized platform name"


mainLoop :: (Bot env, MonadReader (Model env) m, MonadIO m) => m ()
mainLoop = do
  model@Model{..} <- ask
  liftIO $ logDebug logLevel ("Map of user_id and repeat_number:\n" <> prettyShowMap usersSettings)

  liftIO $ logInfo logLevel "Receiving incoming updates."
  income <- liftIO $ getIncome model

  case income of
    Left msg -> liftIO $ logWarning logLevel msg
             >> logInfo logLevel "Waiting 10 seconds before retrying."
             >> threadDelay (1000000 * 10)
             >> runReaderT mainLoop model

    Right incomeWithUpdates -> do
      model' <- liftIO $ handleIncome model incomeWithUpdates
      runReaderT mainLoop model'
