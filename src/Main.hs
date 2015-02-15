module Main where

import Data.Text (Text)
import Network.Wai (pathInfo)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status400)

import qualified Data.Text as Text
import qualified System.Environment as Env

import BasePrelude hiding (app, log)
import Utils

log :: Text -> IO ()
log = putStrLn . Text.unpack

app :: Text -> IO () -> Application
app key tick req respond =
  case pathInfo req of
   ["hook"] -> do
     bytes <- requestBytes req
     hmac <- lift (hmacSha1 key bytes)
     if Just hmac == requestSignature req then
       lift tick >>
       respond (respondText status200 [] "ok")
     else
       respond (respondText status400 [] "Invalid HMAC")

main' :: Int -> Text -> Text -> IO ()
main' port key db = do
  log ("+ Listening on port " <> present port)
  run port (safely $ app key tick)
  where
    tick = do
      gitPull db
      public <- buildPublic db
      promotePublic db public

main :: IO ()
main = do
  args <- Env.getArgs
  let port = read (head args)
  let key = Text.pack (args !! 1)
  let db = Text.pack (args !! 2)
  log ("+ DB " <> db)
  main' port key db
