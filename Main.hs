{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           ClassyPrelude
import           Control.Error.Util
import           Control.Lens
import           Control.Lens.Aeson               (key, values, _String)
import           Control.Monad.Trans.State.Strict
import           Data.Aeson
import           Data.Aeson.Parser                (json')
import           Data.Attoparsec.ByteString       (takeByteString)
import           Data.Attoparsec.Char8            (char, decimal, parseOnly)
import           Network.HTTP.Types
import           Pipes.Attoparsec                 (parse)
import           Pipes.HTTP
import           Prelude                          (read)

data IP = IP Word8 Word8 Word8 Word8 deriving (Show, Eq)

parseIP = do
  one <- decimal
  char '.'
  two <- decimal
  char '.'
  three <- decimal
  char '.'
  four <- decimal
  return $ IP one two three four

getUrl parser headers url = do
  request <- (\r -> r { requestHeaders = headers }) <$> parseUrl url
  withManager tlsManagerSettings $ \manager ->
    withHTTP request manager $ \response ->
      evalStateT (parse parser) (responseBody response)

getPublicIP = getUrl parseIP [] "http://wtfismyip.com/text"

getDNSRecord headers = filter predicate . records <$> json
  where predicate (_, t, n, _) = (t, n) == ("A", "szczyp.com")
        records = zip4
                  <$> scope "content"
                  <*> scope "type"
                  <*> scope "name"
                  <*> scope "record_id"
        scope k = toListOf $ _Right
                  . key "records"
                  . values
                  . key k
                  . _String
        json = getUrl json' headers "https://api.name.com/api/dns/list/szczyp.com"

currentIP = parseOnly parseIP . encodeUtf8 . view (_head . _1)

getHeaders :: IO [Header]
getHeaders = read <$> readFile "headers"

main = do
  headers <- tryAnyDeep getHeaders
  case headers of
    Left _ -> print "can't parse headers for api username and token"
    Right headers -> do
      publicIP <- getPublicIP
      dns <- getDNSRecord headers
      if hush publicIP == (hush $ currentIP dns)
        then print "IP didn't change"
        else do print "IP did change"
                print dns

