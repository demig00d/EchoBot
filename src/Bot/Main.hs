module Bot.Main where

import           System.Environment (getArgs)

import           Bot                (startBot)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> startBot path
    _      -> startBot "config/bot.conf"
