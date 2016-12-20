{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
import Options.Applicative

import Data.Text

import qualified Hypothesis

-- | Overall options
data Options = Options
  { host :: String -- ^ hypothes.is server hostname
  , port :: Int -- ^ hypothes.is server port
  , key :: Text -- ^ Developer API key
  , cmd :: Command
  }

-- | Operation 
data Command = Fetch Hypothesis.AnnotationId
             | Search SearchParams

data SearchParams = SearchParams
  { user :: Maybe Text
  , group :: Maybe Text
  , tag :: Maybe Text
  }

optionParse :: Parser Options
optionParse =
  Options <$> strOption
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
  <*> cmdParse

cmdParse :: Parser Command
cmdParse = subparser $
  command "fetch" (info fetchOpts (progDesc "Fetch annotation by ID")) <>
  command "search" (info searchOpts (progDesc "Search for annotations"))

fetchOpts :: Parser Command
fetchOpts = Fetch <$>
  (pack <$> argument str (metavar "ANNOTATION_ID"
                          <> help "Annotation ID"))

-- | Utility function to parse an optional 'String', returning either
-- 'Nothing' (by default) or else some 'Text'.
parseMaybe :: String -> Parser (Maybe Text)
parseMaybe s = fmap pack <$> option auto (long s <> value Nothing)
-- TODO: The above does not seem to parse strings properly.

searchOpts :: Parser Command
searchOpts = Search <$> (SearchParams
                         <$> parseMaybe "user"
                         <*> parseMaybe "group"
                         <*> parseMaybe "tag")

opts :: ParserInfo Options
opts = info (helper <*> optionParse)
       (fullDesc
         <> progDesc "Fetch annotations from hypothes.is"
         <> header "hypothesis - a test for hypothes.is")

main :: IO ()
main = do
  cfg <- execParser opts
  manager <- newManager tlsManagerSettings
  let baseUrl = BaseUrl Https (host cfg) (port cfg) ""
      k = Just $ key cfg
  case (cmd cfg) of
    Fetch annId -> do
      res <- runExceptT (Hypothesis.annotation k annId manager baseUrl)
      case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right msg -> print msg
    Search (SearchParams user_ group_ tag_) -> do
      res <- runExceptT $
         Hypothesis.search (Just $ key cfg) Nothing Nothing Nothing Nothing Nothing user_ group_ tag_ Nothing manager baseUrl
      case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right msg -> print msg
