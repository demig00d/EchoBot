{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Bot.Telegram (getModel) where

import           Control.Monad              (replicateM_)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Map.Strict            (fromList, insert)
import           Text.Read                  (readMaybe)

import           Bot.Types                  as Bot
import           Logging
import           Telegram.API               as Telegram
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

    eRespBS <- Telegram.sendMethod (logDebug logLevel) token $ GetUpdates offset 25
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
    Update{rCallbackQuery = Just (CallbackQuery cdata cFrom), rUpdateId} ->
      case readMaybe cdata :: Maybe Int of
        Just n -> setRepeatNumber model (uId cFrom) rUpdateId n
        _      -> logWarning (mLogLevel model) ("Can't parse callbackquery: " <> cdata)
               >> pure model

    Update{rMessage = Just message, rUpdateId} ->
      case mText message of
        Just "/start"  -> handleMessage model message rUpdateId ShowStart
        Just "/help"   -> handleMessage model message rUpdateId ShowHelp
        Just "/repeat" -> handleMessage model message rUpdateId ShowRepeat
        _              -> handleMessage model message rUpdateId Echo

    _  -> logWarning' (mLogLevel model) "Can't handle Update" >> pure model


  extractUpdates updates _ = Just updates


setRepeatNumber :: Model TelegramEnv -> Int -> Int -> Int -> IO (Model TelegramEnv)
setRepeatNumber model userId updateId n =
  logInfo' logLevel ("Number of repeats for user: " <> gshow userId <> " changed to " <> gshow n <> ".")
  >> pure model{ mUsersSettings = insert userId n usersSettings
               , mPlatformEnv = platformEnv{offset=updateId + 1}
               }
  where
    usersSettings = mUsersSettings model
    platformEnv= mPlatformEnv model
    logLevel = mLogLevel model


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

    (echoNumber, mUsersSettings') = lookupInsert fromChatId bNumberOfRepeats mUsersSettings

    model' = model{mUsersSettings=mUsersSettings'
                  ,mPlatformEnv=mPlatformEnv{offset = updateId + 1}}

    numKeyboard = mkKeyboard $ fmap (\x -> (show x, show x)) ([1..5] :: [Int])

    Model{..} = model
    TelegramEnv{..} = mPlatformEnv
    BotSettings{..} = mBotSettings

    repeatMessage = "Current number of repeats = " <> gshow echoNumber <> ".\n" <> bRepeatMessage

    sendMessage t cid msg rm = do
      logInfo' mLogLevel "Handling command."
      logInfo' mLogLevel "Send request with 'sendMessage' method to reply to user's command."
      eResponse <- Telegram.sendMethod (logDebug mLogLevel) t $ SendMessage cid msg rm
      case eResponse of
        Left m       -> logWarning mLogLevel m
        Right response -> logInfo' mLogLevel "Reply sended."
                       >> logDebug mLogLevel ("\n" <> response)

    copyMessage t cid fcid mid = do
      logInfo' mLogLevel "Handling message."
      logInfo' mLogLevel ("Number of repeats for user: " <> gshow fcid <> " is " <> gshow echoNumber <> ".")
      let nTimes :: Int -> String
          nTimes = \case
               1   -> gshow (1 :: Int) <> " time"
               num -> gshow num <> " times"
      logInfo mLogLevel ("Send request with 'copyMessage' method " <> nTimes echoNumber <> " to echo user's message.")
      replicateM_ echoNumber $ do
            eResponse <- Telegram.sendMethod (logDebug mLogLevel) t $ CopyMessage cid fcid mid
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

