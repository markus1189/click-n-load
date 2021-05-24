{-# LANGUAGE TypeApplications #-}
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
import Crypto.Error ( maybeCryptoError )
import Crypto.Cipher.AES ( AES128 )
import Crypto.Cipher.Types
    ( BlockCipher(cbcDecrypt), Cipher(cipherInit), makeIV )
import Data.Maybe (fromJust)
import Data.Foldable (traverse_)

type ClickAndLoadAPI = "jdcheck.js" :> Get '[PlainText] Text
                  :<|> "flash" :> "addcrypted2" :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> Post '[PlainText] Text

clickAndLoadAPI :: Proxy ClickAndLoadAPI
clickAndLoadAPI = Proxy

jdcheckHandler :: Handler Text
jdcheckHandler = pure "jdownloader=true"

extractSecret :: Text -> Text -- HACKY way to get key from "javascript" function
extractSecret = Text.takeWhile (/= '\'') . Text.drop 1 . Text.dropWhile (/= '\'')

addcryptedHandler :: MonadIO m => [(Text, Text)] -> m Text
addcryptedHandler kvs =
  liftIO $ case decrypted of
    Nothing -> do
      print kvs
      pure "Failed to decrypt"
    Just result -> do
      putStrLn ("START" <> replicate 75 '=')
      traverse_ (TIO.putStrLn . ("Package:   " <>)) (List.lookup "package" kvs)
      traverse_ (TIO.putStrLn . ("Passwords: " <>)) (List.lookup "passwords" kvs)
      putStrLn (replicate 80 '-')
      TIO.putStrLn . decodeUtf8 $ result
      putStrLn ("END" <> replicate 77 '=')
      pure ""
  where
    decrypted = do
      crypted <- List.lookup "crypted" kvs
      key <- extractSecret <$> List.lookup "jk" kvs
      decryptClickAndLoad key (encodeUtf8 crypted)

decryptClickAndLoad :: Text -> ByteString -> Maybe ByteString
decryptClickAndLoad secret payload = do
  let key = fromJust (Hex.decodeHex secret)
      crypted = decodeLenient payload
  cipher <- maybeCryptoError $ cipherInit @AES128 @ByteString key
  iv <- makeIV @ByteString @AES128 key
  pure $ cbcDecrypt cipher iv crypted

server :: Server ClickAndLoadAPI
server = jdcheckHandler :<|> addcryptedHandler

app :: Application
app = serve clickAndLoadAPI server

appPort :: Int
appPort = 9666

main :: IO ()
main = run appPort app
