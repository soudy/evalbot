--
-- Copyright (C) 2015 soud <soud@protonmail.com>
--
-- Distributed under terms of the MIT license.
--

module Main where

import System.IO
import Network

import IRC
import Config

{-
 - Connect, identify and listen main loop
 -}
main :: IO ()
main = do
    h <- connectTo server (PortNumber (fromIntegral sPort))

    hSetBuffering h NoBuffering
    hSetEncoding  h utf8

    verify h
    listen h

