module Feature.Dose.Types where

import ClassyPrelude
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Feature.Common.Utils

data DoseFilter = DoseFilter{ doseFilterId :: Maybe Integer, doseFilterSex :: Maybe Text} deriving (Eq, Show)

data Dose = Dose{ doseId :: Integer, doseSex :: Text, doseAge :: Text, doseCondition :: Text, 
    doseLot :: Text, doseDate :: Day, doseSerie :: Integer, doseVaccineId :: Integer, 
    doseResidenceJurisdictionId :: Integer, doseResidenceDepartmentId :: Integer, 
    doseApplicationJurisdictionId :: Integer, doseApplicationDepartmentId :: Integer} deriving (Eq, Show)
data CreateDose = CreateDose{ createDoseSex :: Text, createDoseAge :: Text, createDoseCondition :: Text, 
    createDoseLot :: Text, createDoseDate :: Day, createDoseSerie :: Integer, createDoseVaccineId :: Integer, 
    createDoseResidenceJurisdictionId :: Integer, createDoseResidenceDepartmentId :: Integer, 
    createDoseApplicationJurisdictionId :: Integer, createDoseApplicationDepartmentId :: Integer} deriving (Eq, Show)
data DoseError = DoseNameNotFound Text | DoseAlreadyExist Text | UnknownError

newtype DoseWrapper a = DoseWrapper { doseWrapperDose :: a } deriving (Eq, Show)
data DosesWrapper a = DosesWrapper { dosesWrapperDoses :: [a], dosesWrapperCount :: Int } deriving (Eq, Show)

$(commonJSONDeriveMany
    [ ''Dose
    , ''CreateDose
    , ''DoseError
    , ''DoseWrapper
    , ''DosesWrapper
    ])

instance FromRow Dose where
    fromRow = Dose <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> 
        field <*> field <*> field <*> field

instance ToRow CreateDose where
    toRow param =
        [toField $ createDoseSex param, toField $ createDoseAge param, toField $ createDoseCondition param, 
        toField $ createDoseLot param, toField $ createDoseDate param, toField $ createDoseSerie param, 
        toField $ createDoseVaccineId param, toField $ createDoseResidenceJurisdictionId param, 
        toField $ createDoseResidenceDepartmentId param, toField $ createDoseApplicationJurisdictionId param, 
        toField $ createDoseApplicationDepartmentId param]
