module Feature.Date.Types where

import ClassyPrelude
import Database.PostgreSQL.Simple.FromRow
import Platform.Util

data DateFilter = DateFilter{ dateFilterName :: Maybe Text} deriving (Eq, Show)

data DateDose = DateDose{ dateDoseDate :: Day, dateDoseFirstDose :: Integer, 
    dateDoseSecondDose :: Integer, dateDoseThirdDose :: Integer, dateDoseTotalDose :: Integer }
data DateError = UnknownError

newtype DateWrapper a = DateWrapper { dateWrapperDate :: a } deriving (Eq, Show)
data DatesWrapper a = DatesWrapper { datesWrapperDates :: [a], datesWrapperCount :: Int } deriving (Eq, Show)

$(commonJSONDeriveMany
    [ ''DateDose
    , ''DateError
    , ''DateWrapper
    , ''DatesWrapper
    ])

instance FromRow DateDose where
    fromRow = DateDose <$> field <*> field <*> field <*> field <*> field 
