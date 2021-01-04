{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Bot.Telegram (getModel) where

import           Control.Monad              (replicateM_)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Map.Strict            (empty, findWithDefault, insert)
import           Text.Read                  (readMaybe)

import           Bot.Types                  as Bot
import           Logging
import           Requests
import           Telegram.API               as Telegram
import           Telegram.Types
import           Utils                      (eitherDecode, gshow, nTimes)


data TelegramEnv =
  TelegramEnv
    { token  :: String
    , offset :: Int
    } deriving Show


instance Bot TelegramEnv where
  type BotIncome TelegramEnv = [Update]
  type BotUpdate TelegramEnv = Update

  getIncome Model{logLevel,platformEnv=TelegramEnv{..}} = do
    logInfo' logLevel "Send 'getUpdates' method and wait for response."
    let request = Telegram.encodeRequest (logDebug logLevel) token $ GetUpdates offset 25
    eRespBS <- sendPost request
    logInfo' logLevel "Response recieved."
    logDebug logLevel (("\n"<>) <$> eRespBS)
    pure $ do
      respBS <- eRespBS
      resp   <- eitherDecode respBS
      maybe (Left "Failed on getting Update.") Right (rResult resp)


  handleUpdate model = \case
    Update{rCallbackQuery = Just (CallbackQuery cdata cFrom), rUpdateId} ->
      case readMaybe cdata :: Maybe Int of
        Just n -> setRepeatNumber model (uId cFrom) rUpdateId n
        _      -> logWarning (logLevel model) ("Can't parse callbackquery: " <> cdata)
               >> pure model

    Update{rMessage = Just message, rUpdateId} ->
      case mText message of
        Just "/start"  -> handleMessage model message rUpdateId ShowStart
        Just "/help"   -> handleMessage model message rUpdateId ShowHelp
        Just "/repeat" -> handleMessage model message rUpdateId ShowRepeat
        _              -> handleMessage model message rUpdateId Echo

    _  -> logWarning' (logLevel model) "Can't handle Update" >> pure model


  extractUpdates updates _ = Just updates


setRepeatNumber :: Model TelegramEnv -> Int -> Int -> Int -> IO (Model TelegramEnv)
setRepeatNumber model@Model{usersSettings, platformEnv, logLevel} userId updateId n =
  logInfo' logLevel ("Number of repeats for user: " <> gshow userId <> " changed to " <> gshow n <> ".")
  >> pure model{ usersSettings = insert userId n usersSettings
               , platformEnv = platformEnv{offset=updateId + 1}
               }

handleMessage :: Model TelegramEnv -> Message -> Int -> MessageAction -> IO (Model TelegramEnv)
handleMessage model message updateId =
  \case
      ShowStart    -> sendMessage token chatId startMessage Nothing             >> pure model'
      ShowHelp     -> sendMessage token chatId bHelpMessage Nothing             >> pure model'
      ShowRepeat   -> sendMessage token chatId repeatMessage (Just numKeyboard) >> pure model'
      Echo         -> copyMessage token chatId fromChatId messageId             >> pure model'
  where
    chatId     = cId $ mChat message
    fromChatId = uId $ mFrom message
    messageId  = mMessageId  message
    firstName = uFirstName $ mFrom message

    startMessage = "Hi, " <> firstName <> "! " <> bHelpMessage

    echoNumber = findWithDefault bNumberOfRepeats fromChatId usersSettings

    model' = model{platformEnv=platformEnv{offset = updateId + 1}}

    numKeyboard = mkKeyboard $ fmap (\x -> (show x, show x)) ([1..5] :: [Int])

    Model{..} = model
    TelegramEnv{..} = platformEnv
    BotSettings{..} = botSettings

    repeatMessage = "Current number of repeats = " <> gshow echoNumber <> ".\n" <> bRepeatMessage

    sendMessage t cid msg rm = do
      logInfo' logLevel "Handling command."
      logInfo' logLevel "Send request with 'sendMessage' method to reply to user's command."
      let request = Telegram.encodeRequest (logDebug logLevel) t $ SendMessage cid msg rm
      eResponse <- sendPost request
      case eResponse of
        Left m       -> logWarning logLevel m
        Right response -> logInfo' logLevel "Reply sended."
                       >> logDebug logLevel ("\n" <> response)

    copyMessage t cid fcid mid = do
      logInfo' logLevel "Handling message."
      logInfo' logLevel ("Number of repeats for user: " <> gshow fcid <> " is " <> gshow echoNumber <> ".")
      logInfo logLevel ("Send request with 'copyMessage' method " <> nTimes echoNumber <> " to echo user's message.")
      replicateM_ echoNumber $ do
            let request = Telegram.encodeRequest (logDebug logLevel) t $ CopyMessage cid fcid mid
            eResponse <- sendPost request
            case eResponse of
              Left msg       -> logWarning logLevel msg
              Right response -> logInfo' logLevel "Message has been echoed."
                             >> logDebug logLevel ("\n" <> response)


-- | Check request environment and try to get Model from Config.
getModel :: Config -> IO (Either L8.ByteString (Model TelegramEnv))
getModel Config{..} = do
  logInfo' cLogLevel "Send request with 'getMe' method to check token."
  response <- getMe cToken
  case response of
    Left msg  -> do
      logDebug cLogLevel ("\n" <> msg)
      pure . Left  $ "Can't check token, request was unsuccessful."
    Right msg -> do
      logDebug cLogLevel ("\n" <> msg)
      logInfo' cLogLevel "Bot has been found, token is valid."
      pure $ Right model
    where
      model = Model
                { botSettings   = cBotSettings
                , platformEnv   = TelegramEnv cToken 0
                , usersSettings = empty
                , logLevel      = cLogLevel
                }

