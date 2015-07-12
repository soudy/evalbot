--
-- Copyright (C) 2015 soud <soud@protonmail.com>
--
-- Distributed under terms of the MIT license.
--

module IRC
    (
      socket
    , chanmsg
    , privmsg
    , connect
    , run
    , send
    , eval
    , listen
    ) where

import Network
import System.Process
import System.IO
import Data.List       (isPrefixOf, isInfixOf, intercalate)
import Data.List.Split (splitOn)
import Control.Monad.Reader
import Control.Concurrent (threadDelay)

import Config
import Commands.DuckDuckGo

data Bot  = Bot { socket :: Handle }
type Conn = ReaderT Bot IO

{-
 - Send private message to current channel on server
 -}
chanmsg :: String -> Conn ()
chanmsg s = send $ "PRIVMSG " ++ (channel ++ " :" ++ s)

{-
 - Send private message to user
 -}
privmsg :: String -> String -> Conn ()
privmsg u s = send $ "PRIVMSG " ++ (u ++ " :" ++ s)

connect :: IO Bot
connect = do
    h <- connectTo server $ PortNumber (fromIntegral sPort)

    hSetBuffering h NoBuffering
    hSetEncoding  h utf8

    return (Bot h)

{-
 - Verify, join channel(s) and listen forver.
 -}
run :: Conn ()
run = do
    send $ "NICK " ++ nickname
    send $ "USER " ++ (nickname ++ " 0 * :bot")
    send $ "JOIN " ++ channel
    asks socket >>= listen

{-
 - Sending message to connection
 -}
send :: String -> Conn ()
send m = do
    h <- asks socket
    liftIO $ hPutStr h $ m ++ "\r\n"

{-
 - Evaluate message sent
 -}
eval :: String -> String -> Conn ()
eval u m
    | "!big" `isPrefixOf` m = do
        -- At a length of 29, toilet starts on a new line which means 6 messages in
        -- a row, so let's limit the length to 28
        if (length m) > 28
            then chanmsg (u ++ ": That's gonna fill up your screen.")
            else do
                output <- liftIO $ readProcess "toilet" ["-f", "future"] arg
                let messages = splitOn "\n" output

                mapM_ chanmsg $ init messages

    | "!doge" `isPrefixOf` m = chanmsg "https://i.imgur.com/B8qZnEO.gifv"
    | "!ddg"  `isPrefixOf` m = do
        resp <- liftIO $ search arg
        chanmsg $ getAbstractUrl resp
    | "!priv"  `isPrefixOf` m = do
        privmsg u $ "hello there"
  where
    arg = unwords . tail $ splitOn " " m

eval _ _                    = return ()

{-
 - Listen forever and handle messages sent in channel
 -}
listen :: Handle -> Conn ()
listen h = forever $ do
    s <- liftIO $ hGetLine h

    -- Stay connected, respond to pings
    if isInfixOf "PING" s
        then send "PONG"
        else return ()

    -- Eval the messages sent by users in the channel
    if isPrivMsg s
        then eval (getUser s) (getMsg s)
        else return ()
  where
    forever   a = a >> forever a

    isPrivMsg s = isInfixOf "PRIVMSG" s
    getUser   s = (tail . head) $ splitOn "!" s
    getMsg    s = concat $ drop 2 (splitOn ":" s)
