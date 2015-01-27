module Utils (
  Application
, ApplicationM
, lift
, present
, respondText
, requestBody
, requestBytes
, requestSignature
, safely
) where

import Data.ByteString (ByteString)
import Data.Text (Text)

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Network.Wai as W

import BasePrelude hiding (app)
import Control.Monad.Error
import Network.HTTP.Types

data HTTPError = HTTPError Status Text

instance Error HTTPError where
  strMsg = HTTPError status500 . T.pack

type ApplicationM =
  ErrorT HTTPError IO

type Application =
  W.Request -> (W.Response -> ApplicationM W.ResponseReceived) -> ApplicationM W.ResponseReceived

respondText :: Status -> ResponseHeaders -> Text -> W.Response
respondText s h t =
  W.responseLBS s h (TLE.encodeUtf8 . TL.fromChunks $ [t])

requestBytes :: W.Request -> ApplicationM ByteString
requestBytes r =
  lift ((mconcat . BSL.toChunks) <$> W.lazyRequestBody r)

requestBody :: A.FromJSON a => W.Request -> ApplicationM a
requestBody r = do
  body <- lift (W.lazyRequestBody r)
  case A.eitherDecode body of
   Left msg -> throwError (HTTPError status400 ("Invalid JSON: " <> T.pack msg))
   Right a -> return a

(~!!~) :: [a] -> Int -> Maybe a
(~!!~) xs i =
  if i < length xs then
    Just (xs !! i)
  else
    Nothing

requestSignature :: W.Request -> Maybe Text
requestSignature =
     join
   . listToMaybe
   . map ((~!!~ 1) . T.splitOn "=" . TE.decodeUtf8 . snd)
   . filter ((== "X-Hub-Signature") . fst)
   . W.requestHeaders

safely :: Application -> W.Application
safely app req respond = do
  eitherer <- runErrorT (app req (lift . respond))
  case eitherer of
   Left (HTTPError status message) ->
     respond (respondText status [] message)
   Right received ->
     return received

present :: Show a => a -> Text
present = T.pack . show
