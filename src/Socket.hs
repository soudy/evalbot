--
-- Copyright (C) 2015 soud <soud@protonmail.com>
--
-- Distributed under terms of the MIT license.
--

module Socket
    (
      send
    ) where

import Data.List       (isInfixOf)
import Data.List.Split (splitOn)
import System.IO

send :: Handle -> String -> IO ()
send h m = do
    hPutStr h $ m ++ "\r\n"
