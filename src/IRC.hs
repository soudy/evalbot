--
-- Copyright (C) 2015 soud <soud@protonmail.com>
--
-- Distributed under terms of the MIT license.
--

module IRC
    (
      eval
    , privmsg
    , verify
    , listen
    ) where

import Network
import Network.HTTP
import System.Process
import System.IO
import Data.List       (isPrefixOf, isInfixOf, intercalate)
import Data.List.Split (splitOn)

import Socket
import Config

{-
 - Send private message to current channel on server
 -}
privmsg :: Handle -> String -> IO ()
privmsg h s = send h $ "PRIVMSG " ++ (channel ++ " :" ++ s)

verify :: Handle -> IO ()
verify h = do
    send h $ "NICK " ++ nickname
    send h $ "USER " ++ (nickname ++ " 0 * :bot")
    send h $ "JOIN " ++ channel

{-
 - Evaluate message sent
 -}
eval :: Handle -> String -> String -> IO ()
eval h u m
    | "!big" `isPrefixOf` m = do
        -- At a length of 29, toilet starts on a new line which means 6 messages in
        -- a row, so let's limit the length to 28
        if (length m) > 28
            then privmsg h (u ++ ": That's gonna fill up your screen.")
            else do
                let arg = (unwords . tail) $ splitOn " " m
                output <- readProcess "toilet" ["-f", "future"] arg
                let messages = splitOn "\n" output

                mapM_ (privmsg h) (init messages)

    | "!doge"  `isPrefixOf` m = privmsg h "https://i.imgur.com/B8qZnEO.gifv"
    | "!ddg"   `isPrefixOf` m = do
        let arg   = tail (splitOn " " m)
        -- Init twice to remove \r. I don't know how I feel about this method
        let query = init $ init (intercalate "+" arg)

        resp <- simpleHTTP (getRequest $
                            "http://api.duckduckgo.com/?q=" ++ query ++
                            "&format=json&no_html=1&t=evalbot"
                            ) >>= getResponseBody

        privmsg h "ok"

eval _ _ _                         = return ()

{-
 - Listen forever and handle messages sent in channel
 -}
listen :: Handle -> IO ()
listen h = forever $ do
    s <- hGetLine h

    -- Stay connected, respond to pings
    if ping s
        then pong
        else return ()

    -- Eval the messages sent by users in the channel
    if isPrivMsg s
        then eval h (getUser s) (getMsg s)
        else return ()
  where
    forever   a = a >> forever a

    isPrivMsg s = isInfixOf "PRIVMSG" s
    getUser   s = (tail . head) $ splitOn "!" s
    getMsg    s = concat (drop 2 (splitOn ":" s))

    ping      s = isInfixOf "PING" s
    pong        = send h "PONG"
