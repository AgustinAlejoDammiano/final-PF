module Feature.Jurisdiction.Types where

import ClassyPrelude
import Database.PostgreSQL.Simple.FromRow
import Platform.Util

data JurisdictionFilter = JurisdictionFilter{ jurisdictionFilterName :: Maybe Text} deriving (Eq, Show)

data Jurisdiction = Jurisdiction{ jurisdictionId :: Integer, jurisdictionName :: Text} deriving (Eq, Show)
newtype CreateJurisdiction = CreateJurisdiction{ createJurisdictionName :: Text} deriving (Eq, Show)
data JurisdictionError = JurisdictionNotFound Integer | JurisdictionNameNotFound Text

newtype JurisdictionWrapper a = JurisdictionWrapper { jurisdictionWrapperJurisdiction :: a } deriving (Eq, Show)
data JurisdictionsWrapper a = JurisdictionsWrapper { jurisdictionsWrapperJurisdictions :: [a], jurisdictionsWrapperCount :: Int } deriving (Eq, Show)

$(commonJSONDeriveMany
  [ ''Jurisdiction
  , ''CreateJurisdiction
  , ''JurisdictionError
  , ''JurisdictionWrapper
  , ''JurisdictionsWrapper
  ])

instance FromRow Jurisdiction where
  fromRow = Jurisdiction <$> field <*> field
