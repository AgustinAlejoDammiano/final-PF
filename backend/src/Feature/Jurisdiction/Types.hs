module Feature.Jurisdiction.Types where

import ClassyPrelude
import Database.PostgreSQL.Simple.FromRow
import Feature.Common.Utils

data JurisdictionFilter = JurisdictionFilter{ jurisdictionFilterName :: Maybe Text} deriving (Eq, Show)

data Jurisdiction = Jurisdiction{ jurisdictionId :: Integer, jurisdictionName :: Text} deriving (Eq, Show)
data JurisdictionDose = JurisdictionDose{ jurisdictionDoseId :: Integer, jurisdictionDoseName :: Text, 
    jurisdictionDoseFirstDose :: Integer, jurisdictionDoseSecondDose :: Integer, jurisdictionDoseThirdDose :: Integer,
    jurisdictionDoseTotalDose :: Integer }
data CreateJurisdiction = CreateJurisdiction{ createJurisdictionId :: Integer, createJurisdictionName :: Text} deriving (Eq, Show)
data JurisdictionError = JurisdictionNameNotFound Text | JurisdictionAlreadyExist Text | UnknownError

newtype JurisdictionWrapper a = JurisdictionWrapper { jurisdictionWrapperJurisdiction :: a } deriving (Eq, Show)
data JurisdictionsWrapper a = JurisdictionsWrapper { jurisdictionsWrapperJurisdictions :: [a], jurisdictionsWrapperCount :: Int } deriving (Eq, Show)

$(commonJSONDeriveMany
    [ ''Jurisdiction
    , ''JurisdictionDose
    , ''CreateJurisdiction
    , ''JurisdictionError
    , ''JurisdictionWrapper
    , ''JurisdictionsWrapper
    ])

instance FromRow Jurisdiction where
    fromRow = Jurisdiction <$> field <*> field

instance FromRow JurisdictionDose where
    fromRow = JurisdictionDose <$> field <*> field <*> field <*> field <*> field <*> field
