module Feature.Vaccine.Types where

import ClassyPrelude
import Database.PostgreSQL.Simple.FromRow
import Feature.Common.Utils

data VaccineFilter = VaccineFilter{ vaccineFilterName :: Maybe Text} deriving (Eq, Show)

data Vaccine = Vaccine{ vaccineId :: Integer, vaccineName :: Text} deriving (Eq, Show)
data CreateVaccine = CreateVaccine{ createVaccineName :: Text} deriving (Eq, Show)
data VaccineError = VaccineNotFound Integer | VaccineNameNotFound Text | VaccineAlreadyExist Text | UnknownError

newtype VaccineWrapper a = VaccineWrapper { vaccineWrapperVaccine :: a } deriving (Eq, Show)
data VaccinesWrapper a = VaccinesWrapper { vaccinesWrapperVaccines :: [a], vaccinesWrapperCount :: Int } deriving (Eq, Show)

$(commonJSONDeriveMany
    [ ''Vaccine
    , ''CreateVaccine
    , ''VaccineError
    , ''VaccineWrapper
    , ''VaccinesWrapper
    ])

instance FromRow Vaccine where
    fromRow = Vaccine <$> field <*> field
