{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           ClassyPrelude                     hiding (log)
import           Control.Error.Util
import           Control.Lens                      hiding ((.=))
import           Control.Lens.Aeson                (key, values, _String)
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Writer.Strict
import           Data.Aeson
import           Data.Aeson.Parser                 (json')
import           Data.Attoparsec.ByteString        (takeByteString)
import           Data.Attoparsec.Char8             (char, decimal, parseOnly)
import           Data.Attoparsec.Types
import           Data.ByteString.Lazy              (empty)
import           Data.EitherR
import           Network.HTTP.Client
import           Network.HTTP.Types
import           Pipes.Attoparsec                  (parse)
import           Pipes.HTTP
import           Prelude                           (read)
import           System.Environment

main :: IO ()
main = do
  config <- readConfig
  either configError runApp config
  where configError = const $ print "config error"
        runApp config = runEitherT (execWriterT (runReaderT app config))
                        >>= mapM_ print . either (return . pack) id

type App = ReaderT AppConfig (WriterT [Text] (EitherT String IO))

liftIOEither :: IO (Either String a) -> App a
liftIOEither = liftIO >=> liftEither

liftEither :: Either String a -> App a
liftEither = lift . lift . hoistEither

log :: Text -> App ()
log t = lift $ tell [t]

data AppConfig = AppConfig { configHeaders :: [Header]
                           , apiURL        :: String
                           , domain        :: String }
                 deriving Read

app :: App ()
app = do
  publicIP <- liftIOEither requestPublicIP
  dns <- requestDNSRecord
  ip <- liftEither $ getIP dns
  if ip == publicIP
    then do log "IP didn't change"
    else do deleteDNSRecord dns
            postDNSRecord publicIP
            log $ "IP changed from: " ++ ipToText ip ++ " to: " ++ ipToText publicIP

requestPublicIP :: IO (Either String IP)
requestPublicIP = request "http://wtfismyip.com/text" methodGet [] emptyBody parseIP

type DNSRecord = (Text, Text, Text, Text)

requestDNSRecord :: App [DNSRecord]
requestDNSRecord = do
  domain <- asks $ pack . domain
  filter (predicate domain) . records <$> withAPI methodGet emptyBody
  where predicate domain (_, t, n, _) = (t, n) == ("A", domain)
        records = zip4
                  <$> scope "content"
                  <*> scope "type"
                  <*> scope "name"
                  <*> scope "record_id"
        scope k = toListOf $ key "records" . values . key k . _String

deleteDNSRecord :: [DNSRecord] -> App Text
deleteDNSRecord dns = message <$> withAPI methodDelete body
  where body = RequestBodyLBS
               . encode
               . object
               $ [("record_id", String (dns ^. _head . _4))]

postDNSRecord :: IP -> App Text
postDNSRecord ip = message <$> withAPI methodPost body
  where body = RequestBodyLBS
               . encode
               . object
               $ [("type", String "A")
                 ,("hostname" , String "")
                 ,("content", String . ipToText $ ip)
                 ,("ttl", Number 300)]

withAPI :: ByteString -> RequestBody -> App Value
withAPI method body = do
  AppConfig headers baseUrl domain <- ask
  methodUrl <- liftEither . note "invalid method" $ lookup method methodMap
  liftIOEither $ request
    (baseUrl ++ methodUrl ++ "/" ++ domain)
    method
    ([("Content-type", "application/json")] ++ headers)
    body
    json'
  where methodMap :: Map Method String
        methodMap = mapFromList [(methodDelete, "delete")
                                ,(methodPost, "create")
                                ,(methodGet, "list")]

message :: Value -> Text
message = view $ key "result" . key "message" . _String

request :: String
        -> Method
        -> [Header]
        -> RequestBody
        -> Parser ByteString a
        -> IO (Either String a)
request url method headers body parser = do
  r <- parseUrl url
  let req = r { method = method, requestHeaders = headers, requestBody = body }
  withManager tlsManagerSettings $ \manager ->
    withHTTP req manager $ \response ->
      fmapL (const $ "error parsing " ++ url) <$> evalStateT (parse parser) (responseBody response)


getIP :: [DNSRecord] -> Either String IP
getIP = parseOnly parseIP . encodeUtf8 . view (_head . _1)

emptyBody :: RequestBody
emptyBody = RequestBodyLBS empty

data IP = IP Word8 Word8 Word8 Word8 deriving (Show, Eq)

ipToText :: IP -> Text
ipToText (IP a b c d) = pack . join . intersperse "." . map show $ [a, b, c, d]

parseIP :: Parser ByteString IP
parseIP = do
  a <- decimal
  char '.'
  b <- decimal
  char '.'
  c <- decimal
  char '.'
  d <- decimal
  return $ IP a b c d

readConfig :: IO (Either SomeException AppConfig)
readConfig = tryAny $ do
  root <- getExecutablePath
  contents <- readFile $ directory (fpFromString root) </> fpFromText "config"
  return $ read contents

readConfig' :: IO (Either SomeException AppConfig)
readConfig' = tryAny $ read <$> readFile "config"
