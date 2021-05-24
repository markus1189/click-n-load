{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PackageImports #-}

module Main where

import Control.Monad.Reader ( MonadIO(..) )
import qualified "cipher-aes" Crypto.Cipher.AES as AES
-- import Crypto.
import                        Data.ByteString (ByteString)
import                        Data.ByteString.Base64 (decodeLenient)
import                        Data.Text (Text)
import qualified              Data.Text as Text
import                        Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.Wai ( Application )
import Network.Wai.Handler.Warp ( run )
import Servant
    ( Proxy(..),
      serve,
      type (:<|>)(..),
      FormUrlEncoded,
      PlainText,
      ReqBody,
      type (:>),
      Get,
      Post,
      Server,
      Handler )
import qualified              Text.Hex as Hex
import qualified Data.List as List
import qualified Data.Text.IO as TIO

type ClickAndLoadAPI = "jdcheck.js" :> Get '[PlainText] Text
                  :<|> "flash" :> "addcrypted2" :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> Post '[PlainText] Text

clickAndLoadAPI :: Proxy ClickAndLoadAPI
clickAndLoadAPI = Proxy

jdcheckHandler :: Handler Text
jdcheckHandler = pure "jdownloader=true"

addcryptedHandler :: MonadIO m => [(Text, Text)] -> m Text
addcryptedHandler kvs = do
  let Just crypted = List.lookup "crypted" kvs
      Just theKey = Text.takeWhile (/= '\'') . Text.drop 1 . Text.dropWhile (/= '\'') <$> List.lookup "jk" kvs
      decrypted = decryptCnL theKey (encodeUtf8 crypted)
  liftIO $ print kvs
  liftIO . TIO.putStrLn . decodeUtf8 $ decrypted
  pure ""

decryptCnL :: Text -> ByteString -> ByteString
decryptCnL aesKey payload = r
  where crypted = decodeLenient payload
        Just key = Hex.decodeHex aesKey
        aes = AES.initAES key
        iv = AES.aesIV_ key
        r = AES.decryptCBC aes iv crypted

server :: Server ClickAndLoadAPI
server = jdcheckHandler :<|> addcryptedHandler

app :: Application
app = serve clickAndLoadAPI server

main :: IO ()
main = do
  let port = 9666
  putStrLn $ " Listening on localhost:" <> show port
  run port app
