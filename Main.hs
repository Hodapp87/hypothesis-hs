{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
import Options.Applicative

import Data.Text

import qualified Hypothesis

data Config = Config
  { host :: String -- ^ hypothes.is server hostname
  , port :: Int -- ^ hypothes.is server port
  , key :: Text -- ^ Developer API key
  }

config :: Parser Config
config = Config <$> strOption
         (long "host"
           <> value "hypothes.is"
           <> metavar "HOSTNAME"
           <> help "hypothes.is server hostname")
         <*> option auto
         (long "port"
          <> value 443
          <> metavar "PORT"
          <> help "hypothes.is server port")
         <*> (pack <$> strOption
              (long "key"
               <> value ""
               <> metavar "KEY"
               <> help "Developer API key"))

endpoint :: BaseUrl
endpoint = BaseUrl Https "hypothes.is" 443 ""

main :: IO ()
main = do
  let opts = info (helper <*> config)
        (fullDesc
          <> progDesc "Fetch annotations from hypothes.is"
          <> header "hypothesis - a test for hypothes.is")
  cfg <- execParser opts
  manager <- newManager tlsManagerSettings
  --res <- runExceptT (Hypothesis.annotation (Just key) annotId manager endpoint)
  let baseUrl = BaseUrl Https (host cfg) (port cfg) ""
  res <- runExceptT $
         Hypothesis.search (Just $ key cfg) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing manager endpoint
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right msg -> print msg
