{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Bot.VKontakte where

import           Control.Applicative        ((<|>))
import           Control.Monad              (replicateM_)
import           Data.Aeson                 (encode)
import qualified Data.ByteString.Lazy.Char8 as L8 (ByteString)
import           Data.Function              ((&))
import           Data.Functor               ((<&>))
import           Data.Map.Strict            (fromList, insert)
import           Data.Text                  as T (Text, intercalate)

import           Bot.Types
import           Logging
import           Utils
import           VKontakte.API              as VKontakte
import           VKontakte.Types


apiVersion = "5.126"

data VKontakteEnv =
  VKontakteEnv
    { token   :: String
    , groupId :: String
    , server  :: String
    , key     :: String
    , ts      :: String
    } deriving Show


instance Bot VKontakteEnv where
  type BotIncome VKontakteEnv = Response
  type BotUpdate VKontakteEnv = Update

  getIncome Model{..} = do
    let method = UpdatesGet
          { uServer = Just $ server mPlatformEnv
          , uKey = key mPlatformEnv
          , uAct = "a_check"
          , uWait = 25
          , uTs = ts mPlatformEnv
          }
    eResponse <- VKontakte.sendMethod (logDebug mLogLevel) method
    logDebug mLogLevel eResponse
    pure $ eResponse >>= eitherDecode

  handleUpdate model update =
    case oMessage $ uObject update of
      Just msg -> handleMessage model msg
      _        ->
        if uType update == "message_reply"
           then pure model
           else do
             logWarning' (mLogLevel model) "There is no message in Update."
             pure model

  updateModel model@Model{mPlatformEnv=mPlatformEnv} response =
    case rTs response of
      Nothing        -> model
      Just timestamp ->
        let mPlatformEnv' = mPlatformEnv{ts = timestamp}
        in model{mPlatformEnv=mPlatformEnv'}

  extractUpdates income _ = rUpdates income


handleMessage model@Model{..} Message{..} =
  case mText of
    "/1" -> setRepeatsNumber 1 model
    "/2" -> setRepeatsNumber 2 model
    "/3" -> setRepeatsNumber 3 model
    "/4" -> setRepeatsNumber 4 model
    "/5" -> setRepeatsNumber 5 model
    "/help"   -> sendMethod 1 messagesSend{mMessage=Just bHelpMessage}
    "/repeat" -> sendMethod 1 messagesSend{mMessage=Just repeatMessage
                                          ,mKeyboard=Just $ encode numKeyboard}
    _         -> let attachment =
                      fmap toAttachmentFormat mAttachments
                      & filter (/=Nothing)
                      & sequence
                      <&> T.intercalate ","
                 in sendMethod n messagesSend{mMessage=Just mText
                                             ,mStickerId=getStickerId mAttachments
                                             ,mAttachment=attachment}
  where
    BotSettings{..} = mBotSettings
    VKontakteEnv{..} = mPlatformEnv

    getStickerId [] = Nothing
    getStickerId l  = sStickerId <$> (aSticker $ head l)

    repeatMessage = "Current number of repeats = " <> gshow n <> ".\n" <> bRepeatMessage

    numKeyboard =
      Keyboard
        { kOneTime = True
        , kButtons = [fmap mkButton (gshow <$> [1..5])]
        }

    (n, mUsersSettings') = lookupInsert mFromId bNumberOfRepeats mUsersSettings

    setRepeatsNumber number model =
      logInfo' mLogLevel ("Number of repeats for user: " <> gshow mFromId <> " changed to " <> gshow n <> ".")
      >> pure model{mUsersSettings=insert mFromId number mUsersSettings}


    sendMethod :: Int -> Method -> IO (Model VKontakteEnv)
    sendMethod num m = do
      replicateM_ num $ do
       result <- VKontakte.sendMethod (logDebug mLogLevel) m
       case result of
          Left msg    -> logWarning mLogLevel msg
          Right resp  -> logInfo' mLogLevel "Message has been echoed."
                   >> logDebug mLogLevel ("\n" <> resp)

      pure model{mUsersSettings=mUsersSettings'}


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


getModel :: Config -> IO (Either L8.ByteString (Model VKontakteEnv))
getModel Config{cGroupId = Just groupId,..} = do
    rawResponse <- VKontakte.sendMethod (logDebug cLogLevel)$
                      GetLongPollServer
                          { gAccessToken = cToken
                          , gGroupId = groupId
                          , gV = apiVersion
                          }
    logDebug cLogLevel rawResponse
    pure $ do
      rawResponse' <- rawResponse
      resp <- eitherDecode rawResponse'
      skt <- maybe (Left "Failed on getting Update.") Right (rResponse resp)
      pure $ model skt
  where
    model serverKeyTs = Model
              { mBotSettings   = cBotSettings
              , mPlatformEnv   = platformEnv serverKeyTs
              , mUsersSettings = fromList []
              , mLogLevel      = cLogLevel
              }
    platformEnv ServerKeyTs{..} =
      VKontakteEnv
        { token   = cToken
        , groupId = groupId
        , server  = sServer
        , key     = sKey
        , ts      = sTs
        }
