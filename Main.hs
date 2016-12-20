{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client

import Data.Text

import qualified Hypothesis

endpoint :: BaseUrl
endpoint = BaseUrl Https "hypothes.is" 443 ""

-- See https://hypothes.is/account/developer
key :: Text
key = "nope"

annotId :: Text
annotId = "_BlJDsYvEeav_R9rNP_9VA"

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  res <- runExceptT (Hypothesis.annotation (Just key) annotId manager endpoint)
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right msg -> print msg
