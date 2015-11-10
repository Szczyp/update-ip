{-# LANGUAGE DeriveGeneric, FlexibleContexts, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings, TypeFamilies #-}

module Main where

import ClassyPrelude
import Control.Concurrent.Async.Lifted
import Control.Lens
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Trans.Control
import Control.Monad.Writer.Strict
import Data.Aeson
import Data.Aeson.Lens
import Data.Attoparsec.ByteString.Char8
import Network.Wreq                     hiding (get, getWith, postWith)
import Network.Wreq.Session
import Network.Wreq.Types               (Postable)
import System.Environment
import System.FilePath

import qualified Data.CaseInsensitive as CI

newtype App a = App (ReaderT Config (WriterT Log (ExceptT AppError IO)) a)
              deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config
                       , MonadWriter Log, MonadError AppError, MonadBase IO)

instance MonadBaseControl IO App where
  type StM App a = Either AppError (a, Log)
  liftBaseWith f = App $ liftBaseWith $ \r -> f (r . unApp)
    where unApp (App a) = a
  restoreM = App . restoreM

newtype AppError = AppError Text
instance Show AppError where
  show (AppError e) = "Error: " ++ unpack e

data ApiConfig = ApiConfig { configHeaders   :: [(Text, Text)]
                           , configApiUrl    :: String
                           , configDomain    :: Text
                           , configSubDomain :: Text }
                 deriving (Show, Generic)
instance FromJSON ApiConfig

data Config = Config { configSession :: Session
                     , configApi     :: ApiConfig }
              deriving Show

type Log = [Text]

type DNSRecord = (Text, Text, Text, Text)

data IP = IP Word8 Word8 Word8 Word8 deriving (Show, Eq)

ipToText :: IP -> Text
ipToText (IP a b c d) = pack . join . intersperse "." . map show $ [a, b, c, d]

throw :: MonadError AppError m => Text -> m a
throw = throwError . AppError

parseIP :: MonadError AppError m => LByteString -> m IP
parseIP = either (const $ throw "can't parse IP") return
          . over _Left (AppError . pack) . eitherResult . parse parser . toStrict
  where parser = IP <$> decimalDot <*> decimalDot <*> decimalDot <*> decimal
        decimalDot = decimal <* char '.'

getPublicIP :: (MonadReader Config m, MonadError AppError m, MonadIO m) => m IP
getPublicIP = do
  sess <- configSession <$> ask
  view responseBody <$> liftIO (get sess "https://wtfismyip.com/text") >>= parseIP

apiHeaders :: MonadReader Config m => m Options
apiHeaders = do
  hs <- map (over _1 CI.mk . over each encodeUtf8) . configHeaders . configApi <$> ask
  return $ over headers (++ ("Content-type", "application/json") : hs) defaults

apiUrl :: MonadReader Config m => String -> m String
apiUrl url = do
  (aUrl, domain) <- ((,) <$> configApiUrl <*> configDomain) . configApi <$> ask
  return $ aUrl ++ url ++ "/" ++ unpack domain

apiDomain :: MonadReader Config m => m (Text, Text)
apiDomain = ((,) <$> configDomain <*> configSubDomain) . configApi <$> ask

withApi :: (MonadIO m, MonadReader Config m, MonadError AppError m, AsValue a) =>
           (Options -> Session -> String -> IO (Response a)) -> String -> m a
withApi method actionUrl = do
  sess <- configSession <$> ask
  opts <- apiHeaders
  url <- apiUrl actionUrl
  r <- view responseBody <$> liftIO (method opts sess url)
  let code = r ^? key "result" . key "code" . _Number
      msg = r ^? key "result" . key "message" . _String
  case code of
    Just 100 -> return r
    _ -> throw $ fromMaybe "Unknown error" msg

apiGET :: (MonadIO m, MonadReader Config m, MonadError AppError m) => String -> m LByteString
apiGET = withApi getWith

apiPOST :: (MonadIO m, MonadReader Config m, MonadError AppError m, Postable a) =>
           a -> String -> m LByteString
apiPOST body = withApi $ \o s u -> postWith o s u body

message :: LByteString -> Text
message = view $ key "result" . key "message" . _String

getIP :: (MonadError AppError m, MonadIO m) => [DNSRecord] -> m IP
getIP = parseIP . (++ "\n") . fromStrict . encodeUtf8 . view (_head . _1)

readConfig :: (MonadError AppError m, MonadBaseControl IO m, MonadIO m) => m ApiConfig
readConfig = (do root <- takeDirectory <$> liftIO getExecutablePath
                 readFile (root </> "config.json")) `catchAny` fileError
             >>= maybe parseError return . decode
  where fileError = const $ throw "Can't find config file"
        parseError = throw "Can't parse config file"

readConfig' :: (MonadError AppError m, MonadBaseControl IO m, MonadIO m) => m ApiConfig
readConfig' = readFile "config.json" `catchAny` fileError
              >>= maybe parseError return . decode
              where fileError = const $ throw "Can't find config file"
                    parseError = throw "Can't parse config file"

getDNSRecord :: App [DNSRecord]
getDNSRecord = do
  (domain, subDomain) <- apiDomain
  let predicate (_, t, n, _) = (t, n) == ("A", if subDomain == ""
                                               then domain
                                               else subDomain ++ "." ++ domain)
  filter predicate . records <$> apiGET "list"
  where records = zip4
                  <$> scope "content"
                  <*> scope "type"
                  <*> scope "name"
                  <*> scope "record_id"
        scope k = toListOf $ key "records" . values . key k . _String

deleteDNSRecord :: [DNSRecord] -> App Text
deleteDNSRecord dns = message <$> apiPOST body "delete"
  where body = object [("record_id", String (dns ^. _head . _4))]

createDNSRecord :: IP -> App Text
createDNSRecord ip = do
  subDomain <- snd <$> apiDomain
  let body = object [("type", String "A")
                    ,("hostname" , String subDomain)
                    ,("content", String $ ipToText ip)
                    ,("ttl", Number 300)]
  message <$> apiPOST body "create"

mainApp :: App ()
mainApp = do
  (publicIP, dns) <- runConcurrently $ (,)
                     <$> Concurrently getPublicIP
                     <*> Concurrently getDNSRecord
  if null dns
    then do createDNSRecord publicIP
            tell ["New record with IP: " ++ ipToText publicIP]
    else do ip <- getIP dns
            if ip == publicIP
              then tell ["IP didn't change: " ++ ipToText ip]
              else do deleteDNSRecord dns
                      createDNSRecord publicIP
                      tell ["IP changed: " ++ ipToText ip ++ " -> " ++ ipToText publicIP]

runApp :: App a -> Session -> IO (Either AppError (a, Log))
runApp (App app) sess =
  runExceptT $ readConfig' >>= runWriterT . runReaderT app . Config sess

main :: IO ()
main = withSession $ runApp mainApp >=> either print (putStr . unlines . snd)

uip :: App a -> IO (Either AppError (a, Log))
uip = withSession . runApp
