module Spec.Types where

import ClassyPrelude
import Platform.Util
import Control.Monad.Except
import Feature.Common.Types

data Err a
  = ErrMalformedJSON Text
  | ErrInvalidInput InputViolations
  | ErrInternalServerError ByteString
  | ErrUnauthorized Text
  | ErrApp a
  | ErrUnknown Text
  deriving (Eq, Show)
  
data JurisdictionDto = JurisdictionDto{ jurisdictionDtoId :: Text, jurisdictionDtoName :: Text} deriving (Eq, Show)

$(commonJSONDeriveMany
    [ ''JurisdictionDto
    ])
