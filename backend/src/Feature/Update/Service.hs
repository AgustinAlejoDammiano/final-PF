module Feature.Update.Service where

import ClassyPrelude
import Feature.Update.Types
import Feature.Jurisdiction.Types

class Monad m => JurisdictionService m where
    createJurisdiction :: CreateJurisdiction -> m(Either JurisdictionError Jurisdiction)

class Monad m => UpdateRepository m where
    loadData :: Text -> (CreateJurisdiction -> m(Either JurisdictionError Jurisdiction)) -> m(Either UpdateError Update)

update :: (UpdateRepository m, JurisdictionService m) => Text -> m(Either UpdateError Update)
update url = do
    -- TODO delete database? 
    loadData url createJurisdiction
