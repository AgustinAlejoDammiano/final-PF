module Feature.Update.Types where

import ClassyPrelude
import Platform.Util
import Data.Time.LocalTime
import Database.PostgreSQL.Simple.FromRow

data Update = Update{ updateState :: Bool}
data UpdateError = URLNotFound Text | UnknownError
data UpdateDate = UpdateDate{ updateDateDate :: ZonedTime}

newtype UpdateWrapper a = UpdateWrapper { updateWrapperUpdate :: a } deriving (Eq, Show)

$(commonJSONDeriveMany
    [ ''Update
    , ''UpdateError
    , ''UpdateWrapper
    ])

instance FromRow UpdateDate where
    fromRow = UpdateDate <$> field
