{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           ClassyPrelude
import           Control.Error.Util
import           Control.Lens                     hiding ((.=))
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

toText (IP a b c d) = pack . join . intersperse "." . map show $ [a, b, c, d]

parseIP = do
  a <- decimal
  char '.'
  b <- decimal
  char '.'
  c <- decimal
  char '.'
  d <- decimal
  return $ IP a b c d

request url method headers body parser = do
  r <- parseUrl url
  let req = r { requestHeaders = headers, method = method }
  let req' = case body of
        Nothing -> req
        Just body -> req { requestBody = body }
  withManager tlsManagerSettings $ \manager ->
    withHTTP req' manager $ \response ->
      evalStateT (parse parser) (responseBody response)

requestPublicIP = request "http://wtfismyip.com/text" methodGet [] Nothing parseIP

requestDNSRecord headers = filter predicate . records <$> json
  where predicate (_, t, n, _) = (t, n) == ("A", "test.szczyp.com")
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
        json = request "https://api.name.com/api/dns/list/szczyp.com" methodGet headers Nothing json'

deleteDNSRecord headers dns = select <$> result
  where select = toListOf $ _Right . key "result" . key "message" . _String
        result = request
                 "https://api.name.com/api/dns/delete/szczyp.com"
                 methodDelete
                 headers
                 body
                 json'
        body = Just
               . RequestBodyLBS
               . encode
               . object
               $ [("record_id", String (dns ^. _head . _4))]

postDNSRecord headers ip = result
  where result = request
                 "https://api.name.com/api/dns/create/szczyp.com"
                 methodPost
                 headers
                 body
                 json'
        body = Just
               . RequestBodyLBS
               . encode
               . object
               $ [("type", String "A")
                 ,("hostname" , String "test")
                 ,("content", String . toText $ ip)
                 ,("ttl", Number 300)]

getIP = parseOnly parseIP . encodeUtf8 . view (_head . _1)

readHeaders = readFile "headers" >>= tryAnyDeep . return . read

main = do
  headers <- readHeaders
  case headers of
    Left _ -> print "can't parse headers for api username and token"
    Right headers -> do
      publicIP <- requestPublicIP
      dns <- requestDNSRecord headers
      case (publicIP, getIP dns) of
        (Right publicIP, Right ip) | not $ ip == publicIP ->
            do print "IP changed"
               print "deleting dns record"
               deleteDNSRecord headers dns
               print "dns record deleted"
               print "creating new dns record"
               postDNSRecord headers publicIP
               print "new dns record created"

        _ -> print "IP didn't change or cannot parse public ip address or cannot parse dns record"
