{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Bot.Telegram (getModel) where

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Map.Strict            (fromList, insert)
import           Data.Text                  (Text)
import           Text.Read                  (readMaybe)

import           Bot.Types
import           Logging
import           Telegram.API
import qualified Telegram.API               as Telegram (copyMessage,
                                                         getUpdates,
                                                         sendMessage)
import           Telegram.Types
import           Utils                      (eitherDecode, gshow, lookupInsert)


data TelegramEnv =
  TelegramEnv
    { token  :: String
    , offset :: Int
    } deriving Show


instance Bot TelegramEnv where
  type BotIncome TelegramEnv = [Update]
  type BotUpdate TelegramEnv = Update

  getIncome model = do
    logInfo' logLevel "Send 'getUpdates' method and wait for response."

    eRespBS <- Telegram.getUpdates token offset
    logInfo' logLevel "Response recieved."
    logDebug logLevel (("\n"<>) <$> eRespBS)
    pure $ do
      respBS <- eRespBS
      resp   <- eitherDecode respBS
      maybe (Left "Failed on getting Update.") Right (rResult resp)
      where
        TelegramEnv{..} = mPlatformEnv model
        logLevel = mLogLevel model


  handleUpdate model = \case
    updCbq@Update{rCallbackQuery = Just (CallbackQuery cdata _cFrom), ..} ->
      case readMaybe cdata :: Maybe Int of
        Just n -> handleAction model updCbq (SetRepeats n)
        _      -> logWarning (mLogLevel model) ("Can't parse callbackquery: " <> cdata)
               >> pure model

    updMsg@Update{rMessage = Just message} ->
      case mText message of
        Just "/start"  -> handleAction model updMsg ShowStart
        Just "/help"   -> handleAction model updMsg ShowHelp
        Just "/repeat" -> handleAction model updMsg ShowRepeat
        _              -> handleAction model updMsg Echo

    _  -> logWarning' (mLogLevel model) "Can't handle Update" >> pure model


  extractUpdates updates _ = Just updates


data Action
  = ShowStart
  | ShowHelp
  | ShowRepeat
  | Echo
  | SetRepeats Int


handleAction model Update{..} action =
  case rMessage of
    Just message ->
      let chatId     = cId $ mChat message
          fromChatId = uId $ mFrom message
          messageId  = mMessageId  message
          firstName = uFirstName $ mFrom message

          startMessage = "Hi, " <> firstName <> "! " <> bHelpMessage

          (n, mUsersSettings') = lookupInsert fromChatId bNumberOfRepeats mUsersSettings

          mPlatformEnv' = mPlatformEnv{offset = rUpdateId + 1}
          model' = model{mUsersSettings=mUsersSettings', mPlatformEnv=mPlatformEnv'}

          numKeyboard = mkKeyboard $ fmap (\x -> (show x, show x)) [1..5]

      in case action of
        ShowStart    -> sendMessage token chatId startMessage Nothing                 >> pure model'
        ShowHelp     -> sendMessage token chatId bHelpMessage Nothing                 >> pure model'
        ShowRepeat   -> sendMessage token chatId (repeatMessage n) (Just numKeyboard) >> pure model'
        Echo         -> copyMessageN n token chatId fromChatId messageId              >> pure model'
    _           ->
      case rCallbackQuery of
        Just (CallbackQuery cData cFrom) -> case action of
          SetRepeats n ->
            logInfo' mLogLevel ("Number of repeats for user: " <> gshow (uId cFrom) <> " changed to " <> gshow n <> ".")
            >> pure model{ mUsersSettings=insert (uId cFrom) n mUsersSettings
                         , mPlatformEnv=mPlatformEnv{offset=rUpdateId + 1}
                         }
        _         -> logWarning' mLogLevel "Unrecognized update." >> pure model
  where
    Model{..} = model
    TelegramEnv{..} = mPlatformEnv
    BotSettings{..} = mBotSettings

    repeatMessage n = "Current number of repeats = " <> gshow n <> ".\n" <> bRepeatMessage

    sendMessage t cid msg rm = do
      logInfo' mLogLevel "Handle command."
      logInfo' mLogLevel "Send request with 'sendMessage' method to reply to user's command."
      eResponse <- Telegram.sendMessage t cid msg rm
      case eResponse of
        Left msg       -> logWarning mLogLevel msg
        Right response -> logInfo' mLogLevel "Reply sended."
                       >> logDebug mLogLevel ("\n" <> response)

    copyMessageN n t cid fcid mid = do
      logInfo' mLogLevel "Handle message."
      logInfo' mLogLevel ("Number of repeats for user: " <> gshow fcid <> " is " <> gshow n <> ".")
      let nTimes = \case
               1   -> gshow 1 <> " time"
               num -> gshow num <> " times"
      logInfo' mLogLevel ("Send request with 'copyMessage' method " <> nTimes n <> " to echo user's message.")
      replicateM_ n $ do
            eResponse <- Telegram.copyMessage t cid fcid mid
            case eResponse of
              Left msg       -> logWarning mLogLevel msg
              Right response -> logInfo' mLogLevel "Message has been echoed."
                             >> logDebug mLogLevel ("\n" <> response)


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
                { mBotSettings   = cBotSettings
                , mPlatformEnv   = TelegramEnv cToken 0
                , mUsersSettings = fromList []
                , mLogLevel      = cLogLevel
                }
