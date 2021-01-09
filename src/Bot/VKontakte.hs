{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Bot.VKontakte
  ( getModel
  , encodeGetIncome
  , VKontakteEnv(..)
  , Action(..)
  , getAction
  , groupsGetLongPollServer
  ) where

import           Control.Applicative        ((<|>))
import           Control.Monad              (foldM, replicateM_)
import           Data.Aeson                 (encode)
import qualified Data.ByteString.Char8      as S8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8 (ByteString)
import           Data.Function              ((&))
import           Data.Functor               ((<&>))
import           Data.Map.Strict            (empty, findWithDefault)
import           Data.Text                  as T (Text, intercalate)

import           Bot.Types
import           Logging
import           Requests
import           Utils
import           VKontakte.API              (Keyboard (..), Method (..),
                                             groupsGetLongPollServer, mkButton)
import qualified VKontakte.API              as VKontakte (encodeRequest)
import           VKontakte.Types


apiVersion :: String
apiVersion = "5.126"

data VKontakteEnv =
  VKontakteEnv
    { token   :: String
    , groupId :: String
    , server  :: String
    , key     :: String
    , ts      :: String
    } deriving (Show, Eq)


-- Auxiliary types for update handling
data Action
  = SetRepeatNumber Int Int
  | ReplyToCommand Method
  | SendEcho Int Method
  | DoNothing
  deriving (Show, Eq)

instance Bot VKontakteEnv where
  type BotIncome VKontakteEnv = Response

  getIncome model@Model{logLevel} = do
    logInfo' logLevel "Send request to VKontakte server and wait for response."
    let request = encodeGetIncome model

    eResponse <- sendPost request
    case eResponse of
      Left msg       -> pure $ Left msg
      Right response -> do
        logInfo' logLevel "Bot got response."
        logDebug logLevel response
        pure $ eitherDecode response

  handleIncome model@Model{logLevel} income =
    case rUpdates income of
      Nothing      -> logWarning' logLevel "Can't get Updates." >> pure model
      Just []      -> logInfo' logLevel "Updates are empty."    >> pure model
      Just updates -> logInfo' logLevel "Handling updates."
              >> let model' = updateModel model income
              in foldM handleUpdate model' updates


updateModel :: Model VKontakteEnv -> Response -> Model VKontakteEnv
updateModel model@Model{platformEnv=platformEnv} response =
  case rTs response of
    Nothing        -> model
    Just timestamp ->
      let platformEnv' = platformEnv{ts = timestamp}
      in model{platformEnv=platformEnv'}

handleUpdate :: Model VKontakteEnv -> Update -> IO (Model VKontakteEnv)
handleUpdate model update = do
  action <- getAction model update
  case action of
    SetRepeatNumber userId n -> do
      logInfo' (logLevel model) ("Number of repeats for user: " <> gshow userId <> " changed to " <> gshow n <> ".")
      pure $ setRepeatNumber userId n model
    sendAction -> do
      handleSendAction model sendAction
      pure model

getAction :: Logger m => Model VKontakteEnv -> Update -> m Action
getAction model update =
    case oMessage $ uObject update of
      Just msg -> handleMessage model msg
      _        ->
        if uType update == "message_reply"
           then do
             logInfo' (logLevel model) "No messages from user."
             pure DoNothing
           else do
             logWarning' (logLevel model) "There is no message in Update."
             pure DoNothing


handleSendAction :: Model VKontakteEnv -> Action  -> IO ()
handleSendAction Model{logLevel} = \case
  ReplyToCommand method@MessagesSend{} -> do
    logInfo' logLevel "Send request with 'messages.send' method to reply to user's command."
    let request = VKontakte.encodeRequest (logDebug logLevel) method
    result <- sendPost request
    case result of
      Left msg   -> logWarning logLevel msg
      Right resp -> logInfo' logLevel "Reply to command has been sent."
                 >> logDebug logLevel ("\n" <> resp)
  SendEcho echoNumber method@MessagesSend{} -> do
      logInfo logLevel ("Send request with 'messages.send' method " <> nTimes echoNumber <> " to echo user's message.")
      replicateM_ echoNumber $ do
        let request = VKontakte.encodeRequest (logDebug logLevel) method
        result <- sendPost request
        case result of
           Left msg   -> logWarning logLevel msg
           Right resp -> logInfo' logLevel "Message has been echoed."
                      >> logDebug logLevel ("\n" <> resp)


  _ -> pure ()



handleMessage :: Logger m => Model VKontakteEnv -> Message -> m Action
handleMessage Model{..} Message{..} =
  case mText of
    "/1" -> pure $ SetRepeatNumber mFromId 1
    "/2" -> pure $ SetRepeatNumber mFromId 2
    "/3" -> pure $ SetRepeatNumber mFromId 3
    "/4" -> pure $ SetRepeatNumber mFromId 4
    "/5" -> pure $ SetRepeatNumber mFromId 5
    "/help"   -> pure $ ReplyToCommand messagesSend{mMessage=Just bHelpMessage}
    "/repeat" -> pure $ ReplyToCommand messagesSend{mMessage=Just repeatMessage
                                             ,mKeyboard=Just $ encode numKeyboard}
    _         -> do
      let attachment =
              fmap toAttachmentFormat mAttachments
              & filter (/=Nothing)
              & sequence
             <&> T.intercalate ","
      logInfo logLevel ("Message of user '" <> gshow mFromId <> "' would be echoed " <> nTimes echoNumber <> ".")
      pure $ SendEcho echoNumber
               messagesSend{mMessage=Just mText
                           ,mStickerId=getStickerId mAttachments
                           ,mAttachment=attachment}
  where
    BotSettings{..}  = botSettings
    VKontakteEnv{..} = platformEnv

    getStickerId [] = Nothing
    getStickerId l  = sStickerId <$> aSticker (head l)

    repeatMessage = "Current number of repeats = " <> gshow echoNumber <> ".\n" <> bRepeatMessage

    numKeyboard =
      Keyboard
        { kOneTime = True
        , kButtons = [fmap mkButton (gshow <$> ([1..5] :: [Int]))]
        }

    echoNumber = findWithDefault bNumberOfRepeats mFromId usersSettings

    messagesSend =
      MessagesSend
        { mAccessToken = token
        , mGroupId = groupId
        , mUserId  = mFromId
        , mRandomId = mRandomId
        , mMessage = Nothing
        , mAttachment = Nothing
        , mStickerId = Nothing
        , mKeyboard = Nothing
        , mV = apiVersion
        }


toAttachmentFormat :: Attachment -> Maybe Text
toAttachmentFormat Attachment{..} =
        aPhoto *> fmap helper aPhoto
    <|> aVideo *> fmap helper aVideo
    <|> aDoc   *> fmap helper aDoc
    <|> aAudio *> fmap helper aAudio
  where
    helper Media{..} = aType <> gshow mOwnerId <> "_" <> gshow mId <> maybe "" ("_" <>) mAccessKey


getModel ::
  Logger m => Config
  -> (String -> String -> String
  -> (S8.ByteString -> IO ())
  -> m (Either L8.ByteString L8.ByteString))
  -> m (Either L8.ByteString (Model VKontakteEnv))
getModel Config{cGroupId = Nothing} _checkMethod = pure $ Left "group_id is required for VKontakte."
getModel Config{cGroupId = Just groupId, cToken, cLogLevel, cBotSettings} checkMethod = do

  rawResponse <- checkMethod groupId cToken apiVersion (logDebug cLogLevel)

  logDebug cLogLevel rawResponse
  pure $ do
    rawResponse' <- rawResponse
    resp <- eitherDecode rawResponse'
    skt  <- maybe (Left "Failed on getting Update.") Right (rResponse resp)
    pure $ model skt
  where
    model serverKeyTs = Model
              { botSettings   = cBotSettings
              , platformEnv   = platformEnv serverKeyTs
              , usersSettings = empty
              , logLevel      = cLogLevel
              }
    platformEnv ServerKeyTs{..} =
      VKontakteEnv
        { token   = cToken
        , groupId = groupId
        , server  = sServer
        , key     = sKey
        , ts      = sTs
        }


encodeGetIncome :: Model VKontakteEnv -> Requests.Handler
encodeGetIncome Model{logLevel, platformEnv} =
  VKontakte.encodeRequest (logDebug logLevel) method
    where
      method = UpdatesGet
          { uServer = server platformEnv
          , uKey = key platformEnv
          , uAct = "a_check"
          , uWait = 25
          , uTs = ts platformEnv
          }
