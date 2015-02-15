module Utils (
  Application
, ApplicationM
, buildPublic
, hmacSha1
, gitPull
, lift
, present
, promotePublic
, respondText
, requestBody
, requestBytes
, requestSignature
, safely
) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding.Locale (decodeLocale)
import System.Process (CreateProcess(..))

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock as Clock
import qualified Network.Wai as W
import qualified System.FilePath.Posix as FilePath
import qualified System.Process as P

import BasePrelude hiding (app, assert)
import Control.Monad.Error
import Network.HTTP.Types

data HTTPError =
  HTTPError Status Text

instance Error HTTPError where
  strMsg = HTTPError status500 . T.pack

newtype Public =
  Public Text

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

unsafeGetContents :: Handle -> P.ProcessHandle -> Text -> IO ByteString
unsafeGetContents hout hproc tag = do
  contents <- B.hGetContents hout
  code <- P.waitForProcess hproc
  case code of
   ExitSuccess ->
     return contents
   ExitFailure i ->
     (error . T.unpack . msg) i
  where
    msg i =
      "shell: exit code " <> present i <> ": " <> tag

shell :: [Text] -> Maybe ByteString -> IO Text
shell args_ stdinM =
  T.strip <$> (decodeLocale =<< bracket acquire release readOut)
  where
    acquire =
      P.createProcess builder
    release (Just hin, Just hout, _, _) = do
      hClose hin
      hClose hout
    readOut (Just hin, Just hout, _, hproc) = do
      maybe (return ()) (B.hPut hin) stdinM
      hClose hin
      unsafeGetContents hout hproc (present args_)
    builder0 = P.proc (head args) (tail args)
    builder = builder0 { std_in = P.CreatePipe, std_out = P.CreatePipe, std_err = P.CreatePipe }
    args = T.unpack <$> args_

hmacSha1 :: Text -> ByteString -> IO Text
hmacSha1 key message =
  shell ["openssl", "sha1", "-hmac", key] (Just message)

gitPull :: Text -> IO Text
gitPull path =
  shell ["git", "-C", path, "pull"] Nothing

measureDate :: IO (Integer, Integer, Integer, Text)
measureDate = do
  Clock.UTCTime date time <- Clock.getCurrentTime
  let (year, month, day) = Calendar.toGregorian date
  return (year, fromIntegral month, fromIntegral day, present time)

slash :: [Text] -> Text
slash = T.pack . FilePath.joinPath . fmap T.unpack

buildPublic :: Text -> IO Public
buildPublic db = do
  (year, month, day, time) <- measureDate
  let key = present year <> "-" <> present month <> "-" <> present day <> "-" <> time
  shell ["mkdir", slash [db, "public-" <> key]] Nothing
  return (Public key)

promotePublic :: Text -> Public -> IO ()
promotePublic db (Public key) = do
  shell ["ln", "-sf", slash [db, "public-" <> key], slash [db, "public"]] Nothing
  return ()
