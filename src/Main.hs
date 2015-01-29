module Main where

import Data.Text (Text)
import Network.Wai (pathInfo)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status400)

import qualified Data.Text as Text
import qualified System.Environment as Env

import BasePrelude hiding (app, log)
import Utils

log :: String -> IO ()
log = putStrLn

app :: Text -> Application
app key req respond =
  case pathInfo req of
   ["hook"] -> do
     bytes <- requestBytes req
     hmac <- lift (hmacSha1 key bytes)
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
  let key = Text.pack (args !! 1)
  main' port key
