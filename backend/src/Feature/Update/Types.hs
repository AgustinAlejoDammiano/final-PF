module Feature.Update.Types where

import ClassyPrelude
import Feature.Common.Utils
import Data.Time.LocalTime
import Database.PostgreSQL.Simple.FromRow

data Update = Update{ updateState :: Bool}
data UpdateError = UnknownError
data UpdateDate = UpdateDate{ updateDateDate :: LocalTime}

newtype UpdateWrapper a = UpdateWrapper { updateWrapperUpdate :: a } deriving (Eq, Show)
data UpdatesWrapper a = UpdatesWrapper { updatesWrapperUpdates :: [a], updatesWrapperCount :: Int } deriving (Eq, Show)

$(commonJSONDeriveMany
    [ ''Update
    , ''UpdateError
    , ''UpdateDate
    , ''UpdateWrapper
    , ''UpdatesWrapper
    ])

instance FromRow UpdateDate where
    fromRow = UpdateDate <$> field
