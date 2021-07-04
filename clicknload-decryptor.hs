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

import qualified Language.JavaScript.Parser as JS
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
import Language.JavaScript.Parser (JSAST(JSAstProgram), JSStatement (JSFunction, JSReturn), JSExpression (JSStringLiteral))
import Language.JavaScript.Parser.AST (JSBlock(JSBlock))

type ClickAndLoadAPI = "jdcheck.js" :> Get '[PlainText] Text
                  :<|> "flash" :> "addcrypted2" :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> Post '[PlainText] Text
                  :<|> "flash" :> "add" :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> Post '[PlainText] Text

clickAndLoadAPI :: Proxy ClickAndLoadAPI
clickAndLoadAPI = Proxy

jdcheckHandler :: Handler Text
jdcheckHandler = pure "jdownloader=true"

extractSecret :: Text -> Maybe Text -- HACKY way to get key from "javascript" function
extractSecret input = case js of
  JSAstProgram [JSFunction _ _ _ _ _ (JSBlock _ [JSReturn _ (Just (JSStringLiteral _ str)) _] _) _] _ ->
    pure . Text.pack . filter (/= '\'') $ str
  _ -> Nothing
  where js = JS.readJs (Text.unpack input)

addcryptedHandler :: MonadIO m => [(Text, Text)] -> m Text
addcryptedHandler kvs =
  liftIO $ case decrypted of
    Nothing -> do
      putStrLn (replicate 75 '!')
      print kvs
      putStrLn (replicate 75 '!')
      pure "Failed to decrypt"
    Just result -> do
      putStrLn ("START" <> replicate 75 '=')
      traverse_ (TIO.putStrLn . ("Package:   " <>)) (List.lookup "package" kvs)
      traverse_ (TIO.putStrLn . ("Passwords: " <>)) (List.lookup "passwords" kvs)
      traverse_ (TIO.putStrLn . ("jk:        " <>)) (List.lookup "jk" kvs)
      traverse_ (TIO.putStrLn . ("secret:    " <>)) secret
      putStrLn (replicate 80 '-')
      TIO.putStrLn . decodeUtf8 $ result
      putStrLn ("END" <> replicate 77 '=')
      pure ""
  where
    secret = List.lookup "jk" kvs >>= extractSecret
    decrypted = do
      crypted <- List.lookup "crypted" kvs
      key <- secret
      decryptClickAndLoad key (encodeUtf8 crypted)

addHandler :: MonadIO m => [(Text, Text)] -> m Text
addHandler kvs = liftIO $ do
  putStrLn ("START" <> replicate 75 '=')
  traverse_ (TIO.putStrLn . ("Source: " <>)) (List.lookup "source" kvs)
  putStrLn ("END" <> replicate 10 '-')
  traverse_ (traverse_ TIO.putStrLn . Text.words) (List.lookup "urls" kvs)
  putStrLn ("END" <> replicate 77 '=')
  pure ""

decryptClickAndLoad :: Text -> ByteString -> Maybe ByteString
decryptClickAndLoad secret payload = do
  let key = fromJust (Hex.decodeHex secret)
      crypted = decodeLenient payload
  cipher <- maybeCryptoError $ cipherInit @AES128 @ByteString key
  iv <- makeIV @ByteString @AES128 key
  pure $ cbcDecrypt cipher iv crypted

server :: Server ClickAndLoadAPI
server = jdcheckHandler :<|> addcryptedHandler :<|> addHandler

app :: Application
app = serve clickAndLoadAPI server

appPort :: Int
appPort = 9666

main :: IO ()
main = putStrLn ("Click'n'Load Decryptor on port " <> show appPort) >> run appPort app
