{-# LANGUAGE LambdaCase #-}
module Bot (startBot) where

import qualified Data.ByteString.Char8 as S8

import           Configuration


startBot :: FilePath -> IO ()
startBot path = do
  config <- getConfig path
  case config of
    Left message -> putStrLn message
    Right config -> putStrLn "Configuration file has been read and decoded."
