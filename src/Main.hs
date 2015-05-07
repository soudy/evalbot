--
-- Copyright (C) 2015 soud <soud@protonmail.com>
--
-- Distributed under terms of the MIT license.
--

module Main where

import System.IO
import Network
import Control.Monad.Reader
import Control.Exception

import IRC

{-
 - Connect, identify and listen main loop
 -}
main :: IO ()
main = bracket connect dc loop
  where
    dc      = hClose . socket
    loop st = runReaderT run st
