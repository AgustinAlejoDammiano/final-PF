module Feature.Date.Service where

import ClassyPrelude
import Feature.Date.Types
import Feature.Common.Types

class (Monad m) => Dao m where
    listDatesDoseFromDB :: Pagination -> m [DateDose]


listDatesDose :: (Dao m) => Pagination -> m [DateDose]
listDatesDose = listDatesDoseFromDB
