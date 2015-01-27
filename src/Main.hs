module Main where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding.Locale ( decodeLocale)
import Network.Wai (pathInfo)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status400)
import System.Process as P

import qualified Data.ByteString as B
import qualified Data.Text as Text
import qualified System.Environment as Env

import BasePrelude hiding (app, log)
import Utils

log :: String -> IO ()
log = putStrLn

hmacOf :: Text -> ByteString -> IO Text
hmacOf key message =
  bracket acquire release hmacOf'
  where
    acquire =
      P.createProcess builder
    release (Just hin, Just hout, _, _) =
      hClose hin >> hClose hout
    hmacOf' (Just hin, Just hout, _, _) = do
      B.hPut hin message
      hClose hin
      Text.strip <$> (B.hGetContents hout >>= decodeLocale)
    builder0 =
      P.proc "openssl" ["sha1", "-hmac", Text.unpack key]
    builder =
      builder0 { std_in = P.CreatePipe, std_out = P.CreatePipe }

app :: Text -> Application
app key req respond =
  case pathInfo req of
   ["hook"] -> do
     bytes <- requestBytes req
     hmac <- lift (hmacOf key bytes)
     if Just hmac == requestSignature req then
        respond (respondText status200 [] "ok")
     else
        respond (respondText status400 [] "Invalid HMAC")

main' :: Int -> Text -> IO ()
main' port key = do
  log ("+ Listening on port " <> show port)
  run port (safely $ app key)

main :: IO ()
main = do
  args <- Env.getArgs
  let port = read (head args)
  let key = read (args !! 1)
  main' port key
