{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Bot.Telegram
  ( getModel
  , encodeGetIncome
  , TelegramEnv(..)
  , Action(..)
  , handleUpdate
  ) where

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


-- Auxiliary types for update handling
data Action
  = SetRepeatNumber Int Int
  | Send Method
  | DoNothing
  deriving (Show, Eq)

instance Bot TelegramEnv where
  type BotIncome TelegramEnv = [Update]
  type BotUpdate TelegramEnv = Update

  getIncome model@Model{logLevel} = do
    logInfo' logLevel "Send 'getUpdates' method and wait for response."
    let request = encodeGetIncome model

    eRespBS <- sendPost request
    logInfo' logLevel "Response recieved."
    logDebug logLevel (("\n"<>) <$> eRespBS)
    pure $ do
      respBS <- eRespBS
      resp   <- eitherDecode respBS
      maybe (Left "Failed on getting Update.") Right (rResult resp)


  handleIncome model@Model{..} income = do
    let action = handleUpdate model income
    model' <- case action of
      SetRepeatNumber userId n -> do
        logInfo' logLevel ("Number of repeats for user: " <> gshow userId <> " changed to " <> gshow n <> ".")
        pure $ setRepeatNumber userId n model
      sendAction -> do
        handleSendAction model sendAction
        pure model
    pure $ model'{platformEnv = platformEnv{Bot.Telegram.offset=rUpdateId income + 1}}


  extractUpdates updates _ = Just updates


handleUpdate :: Model TelegramEnv -> Update -> Action
handleUpdate model@Model{..} = \case
  Update{rCallbackQuery = Just (CallbackQuery cdata cFrom)} ->
    case readMaybe cdata :: Maybe Int of
      Just n -> SetRepeatNumber (uId cFrom) n
      _      -> DoNothing
  Update{rMessage = Just message} -> handleMessage model message
  _ -> DoNothing

handleMessage :: Model TelegramEnv -> Message -> Action
handleMessage Model{usersSettings, botSettings} message =
    case mText message of
      Just "/start"  -> Send $ SendMessage chatId startMessage  Nothing
      Just "/help"   -> Send $ SendMessage chatId helpMessage   Nothing
      Just "/repeat" -> Send $ SendMessage chatId repeatMessage (Just numKeyboard)
      _              -> Send $ CopyMessage chatId fromChatId messageId
  where
    chatId     = cId $ mChat message
    fromChatId = uId $ mFrom message
    messageId  = mMessageId  message
    firstName  = uFirstName $ mFrom message

    echoNumber = findWithDefault (bNumberOfRepeats botSettings) fromChatId usersSettings

    helpMessage = bHelpMessage botSettings
    startMessage = "Hi, " <> firstName <> "! " <> helpMessage
    repeatMessage = "Current number of repeats = " <> gshow echoNumber <> ".\n" <> bRepeatMessage botSettings

    numKeyboard = mkKeyboard $ fmap (\x -> (show x, show x)) ([1..5] :: [Int])


setRepeatNumber :: Int -> Int -> Model TelegramEnv -> Model TelegramEnv
setRepeatNumber userId n model@Model{usersSettings} =
  model{ usersSettings = insert userId n usersSettings }

handleSendAction :: Model TelegramEnv -> Action -> IO ()
handleSendAction Model{platformEnv=TelegramEnv{token},..} = \case
  Send method@SendMessage{} -> sendMessage method
  Send method@CopyMessage{} -> copyMessage method
  _                         -> pure ()

  where
    echoNumber m = findWithDefault (bNumberOfRepeats botSettings) (chatId m) usersSettings

    sendMessage method = do
      logInfo' logLevel "Handling command."
      logInfo' logLevel "Send request with 'sendMessage' method to reply to user's command."
      let request = Telegram.encodeRequest (logDebug logLevel) token method
      eResponse <- sendPost request
      case eResponse of
        Left m         -> logWarning logLevel m
        Right response -> logInfo' logLevel "Reply sended."
                       >> logDebug logLevel ("\n" <> response)

    copyMessage method = do
      logInfo' logLevel "Handling message."
      logInfo' logLevel ("Number of repeats for user: " <> gshow (chatId method) <> " is " <> gshow (echoNumber method) <> ".")
      logInfo  logLevel ("Send request with 'copyMessage' method " <> nTimes (echoNumber method) <> " to echo user's message.")
      replicateM_ (echoNumber method) $ do
          let request = Telegram.encodeRequest (logDebug logLevel) token method
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


encodeGetIncome :: Model TelegramEnv -> Requests.Handler
encodeGetIncome Model{logLevel, platformEnv=TelegramEnv{..}} =
  Telegram.encodeRequest (logDebug logLevel) token $ GetUpdates offset 25
