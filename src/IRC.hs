--
-- Copyright (C) 2015 soud <soud@protonmail.com>
--
-- Distributed under terms of the MIT license.
--

module IRC
    (
      socket
    , privmsg
    , connect
    , run
    , send
    , eval
    , listen
    ) where

import Network
import Network.HTTP
import System.Process
import System.IO
import Data.List       (isPrefixOf, isInfixOf, intercalate)
import Data.List.Split (splitOn)
import Control.Monad.Reader

import Config

data Bot  = Bot { socket :: Handle }
type Conn = ReaderT Bot IO

{-
 - Send private message to current channel on server
 -}
privmsg :: String -> Conn ()
privmsg s = send $ "PRIVMSG " ++ (channel ++ " :" ++ s)

connect :: IO Bot
connect = do
    h <- connectTo server (PortNumber (fromIntegral sPort))

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
            then privmsg (u ++ ": That's gonna fill up your screen.")
            else do
                let arg = (unwords . tail) $ splitOn " " m
                output <- liftIO $ readProcess "toilet" ["-f", "future"] arg
                let messages = splitOn "\n" output

                mapM_ (privmsg) (init messages)

    | "!doge"  `isPrefixOf` m = privmsg "https://i.imgur.com/B8qZnEO.gifv"
    | "!ddg"   `isPrefixOf` m = do
        let arg   = tail (splitOn " " m)
        -- Init twice to remove \r. I don't know how I feel about this method
        let query = init $ init (intercalate "+" arg)

        resp <- liftIO $ simpleHTTP (getRequest $
                                     "http://api.duckduckgo.com/?q=" ++ query ++
                                     "&format=json&no_html=1&t=evalbot"
                                     ) >>= getResponseBody

        privmsg "ok"

eval _ _                         = return ()

{-
 - Listen forever and handle messages sent in channel
 -}
listen :: Handle -> Conn ()
listen h = forever $ do
    s <- liftIO (hGetLine h)

    -- Stay connected, respond to pings
    if ping s
        then pong
        else return ()

    -- Eval the messages sent by users in the channel
    if isPrivMsg s
        then eval (getUser s) (getMsg s)
        else return ()
  where
    forever   a = a >> forever a

    isPrivMsg s = isInfixOf "PRIVMSG" s
    getUser   s = (tail . head) $ splitOn "!" s
    getMsg    s = concat (drop 2 (splitOn ":" s))

    ping      s = isInfixOf "PING" s
    pong        = send "PONG"
