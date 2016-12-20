{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (mzero)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Servant.API
import Servant.Client

import Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T

-- import Lib

-- | API type for hypothes.is (see
-- <https://h.readthedocs.io/en/latest/api/>)
type HypothesisAPI =
       "annotations" :> Header "Authorization" Text :>
       Capture "id" AnnotationId :> Get '[JSON] Annotation
  :<|> "search" :> Header "Authorization" Text :>
        QueryParam "limit"  Int :>
        QueryParam "offset" Int :>
        QueryParam "sort"   Text :>
        QueryParam "order"  Text :>
        QueryParam "uri"    Text :>
        QueryParam "user"   Text :>
        QueryParam "group"  Text :>
        QueryParam "tag"    Text :> 
        QueryParam "any"    Text :>       
        Get '[JSON] [Annotation]

type AnnotationId = Text

data Annotation = Annotation
  { annotGroup :: Text
  --, annotPermissions :: [String]
  --, annotTarget :: ?
  --, annotReferences :: [Text]
  , annotTags :: [Text]
  , annotText :: Text
  , annotUri :: Text
  , annotId :: AnnotationId
  } deriving (Eq, Show)

instance FromJSON Annotation where
  parseJSON (Object o) =
    Annotation <$> o .: "group"
               <*> o .: "tags"
               <*> o .: "text"
               <*> o .: "uri"
               <*> o .: "id"
  parseJSON _ = mzero

hypothesisAPI :: Proxy HypothesisAPI
hypothesisAPI = Proxy

annotation :: Maybe Text
           -> AnnotationId
           -> Manager
           -> BaseUrl
           -> ExceptT ServantError IO Annotation
search :: Maybe Text
       -> Maybe Int
       -> Maybe Int
       -> Maybe Text
       -> Maybe Text
       -> Maybe Text
       -> Maybe Text
       -> Maybe Text
       -> Maybe Text
       -> Maybe Text
       -> Manager
       -> BaseUrl
       -> ExceptT ServantError IO [Annotation]
annotation :<|> search = client hypothesisAPI

-- (BaseUrl Http "hypothes.is" 80)

main :: IO ()
main = putStrLn "Hello world"
