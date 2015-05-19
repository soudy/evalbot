{-# LANGUAGE OverloadedStrings #-}
--
-- DuckDuckGo.hs
-- Copyright (C) 2015 soud <soud@protonmail.com>
--
-- Distributed under terms of the MIT license.
--

module Commands.DuckDuckGo
    (
      search,
      getAbstractUrl
    ) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Aeson
import Data.List       (intercalate)
import Control.Monad   (mzero)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B

data ApiResponse = ApiResponse { heading     :: Maybe String
                               , abstractUrl :: String
                               } deriving Show

instance FromJSON ApiResponse where
    parseJSON (Object v) = ApiResponse <$>
                           v .:? "Heading"     <*>
                           v .:  "AbstractURL"
    parseJSON _          = mzero

search :: String -> IO String
search query = do
    req <- parseUrl url

    withManager tlsManagerSettings $ \mgr -> do
        resp <- httpLbs req mgr
        return (B.unpack $ responseBody resp)
  where
    url = "https://api.duckduckgo.com/?q=" ++ query ++ "&format=json&no_html=1&t=evalbot"

getAbstractUrl :: String -> String
getAbstractUrl json = do
    abstractUrl jsonResponse
  where
    (Just jsonResponse) = decode $ B.pack json :: Maybe ApiResponse
