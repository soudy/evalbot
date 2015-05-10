{-# LANGUAGE OverloadedStrings #-}
--
-- DuckDuckGo.hs
-- Copyright (C) 2015 soud <soud@protonmail.com>
--
-- Distributed under terms of the MIT license.
--

module DuckDuckGo
    (
      search
    ) where

import Network.HTTP.Conduit
import Data.Aeson
import Data.List       (intercalate)
import Data.List.Split (splitOn)
import Control.Monad   (mzero)

data ApiResponse = ApiResponse { heading     :: Maybe String
                               , abstractUrl :: String
                               } deriving Show

instance FromJSON ApiResponse where
    parseJSON (Object v) = ApiResponse <$>
                           v .:? "Heading"     <*>
                           v .:  "AbstractURL"
    parseJSON _          = mzero

search :: String -> String
search a = do
    resp <- simpleHttp url

    let (Just jsonResponse) = decode resp :: Maybe ApiResponse

    return $ fst $ abstractUrl jsonResponse
  where
    url = "https://api.duckduckgo.com/?q=" ++ a ++ "&format=json&no_html=1&t=evalbot"

main :: IO ()
main = putStr $ search "arch linux"
