{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypothesis where

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

-- | API type for hypothes.is (see
-- <https://h.readthedocs.io/en/latest/api/>)
type HypothesisAPI =
       "api/annotations" :> Header "Authorization" APIKey :>
       Capture "id" AnnotationId :> Get '[JSON] Annotation
  :<|> "api/search" :> Header "Authorization" APIKey :>
        QueryParam "limit"  Int :>
        QueryParam "offset" Int :>
        QueryParam "sort"   Text :>
        QueryParam "order"  Text :>
        QueryParam "uri"    Text :>
        QueryParam "user"   Text :>
        QueryParam "group"  Text :>
        QueryParam "tag"    Text :> 
        QueryParam "any"    Text :>       
        Get '[JSON] AnnotationRows

newtype AnnotationRows = AnnotationRows [Annotation]
  deriving (Show)

type APIKey = Text

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

instance FromJSON AnnotationRows
  where parseJSON (Object o) = (AnnotationRows <$> (o .: "rows"))

hypothesisAPI :: Proxy HypothesisAPI
hypothesisAPI = Proxy

-- | Fetch an annotation by ID. (See
-- <https://h.readthedocs.io/en/latest/api/#operation/fetchAnnotation>)
annotation :: Maybe APIKey -- ^ Auth key
           -> AnnotationId -- ^ Annotation to retrieve
           -> Manager
           -> BaseUrl
           -> ExceptT ServantError IO Annotation

-- | Search annotations. (See
-- <https://h.readthedocs.io/en/latest/api/#operation/search> for
-- definitions of arguments.)
search :: Maybe APIKey -- ^ Auth key
       -> Maybe Int -- ^ Limit
       -> Maybe Int -- ^ Offset
       -> Maybe Text -- ^ Sort
       -> Maybe Text -- ^ Order
       -> Maybe Text -- ^ URI
       -> Maybe Text -- ^ User
       -> Maybe Text -- ^ Group
       -> Maybe Text -- ^ Tag
       -> Maybe Text -- ^ Any
       -> Manager
       -> BaseUrl
       -> ExceptT ServantError IO AnnotationRows

annotation :<|> search = client hypothesisAPI
