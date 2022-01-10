module Feature.Update.Types where

import ClassyPrelude
import Platform.Util

data Update = Update{ updateState :: Bool}
data UpdateError = URLNotFound Text

newtype UpdateWrapper a = UpdateWrapper { updateWrapperUpdate :: a } deriving (Eq, Show)

$(commonJSONDeriveMany
    [ ''Update
    , ''UpdateError
    , ''UpdateWrapper
    ])
    