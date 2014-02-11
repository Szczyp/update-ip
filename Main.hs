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

request url method headers parser = do
  request <- (\r -> r { requestHeaders = headers, method = method }) <$> parseUrl url
  withManager tlsManagerSettings $ \manager ->
    withHTTP request manager $ \response ->
      evalStateT (parse parser) (responseBody response)

requestPublicIP = request "http://wtfismyip.com/text" methodGet [] parseIP

requestDNSRecord headers = filter predicate . records <$> json
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
        json = request  "https://api.name.com/api/dns/list/szczyp.com" methodGet headers json'

ip = parseOnly parseIP . encodeUtf8 . view (_head . _1)

readHeaders = readFile "headers" >>= tryAnyDeep . return . read

main = do
  headers <- readHeaders
  case headers of
    Left _ -> print "can't parse headers for api username and token"
    Right headers -> do
      publicIP <- requestPublicIP
      dns <- requestDNSRecord headers
      if hush publicIP == (hush $ ip dns)
        then print "IP didn't change"
        else do print "IP did change"
                print dns
