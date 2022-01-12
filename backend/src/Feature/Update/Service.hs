module Feature.Update.Service where

import ClassyPrelude
import Feature.Update.Types
import Feature.Common.Types
import Feature.Jurisdiction.Types
import Data.Time.LocalTime

class Monad m => JurisdictionService m where
    createJurisdiction :: CreateJurisdiction -> m(Either JurisdictionError Jurisdiction)

class Monad m => UpdateDao m where
    listUpdatesFromDB :: Pagination -> m [UpdateDate]
    createUpdateFromDB :: ZonedTime -> m (Either UpdateError Update)

class Monad m => UpdateRepository m where
    loadData :: Text -> (CreateJurisdiction -> m(Either JurisdictionError Jurisdiction)) -> m(Either UpdateError Update)

update :: (UpdateRepository m, JurisdictionService m, UpdateDao m,  MonadUnliftIO m) => Text -> m(Either UpdateError Update)
update url = do
    t <- liftIO getZonedTime
    -- TODO delete database? 
    result <- loadData url createJurisdiction
    result' <- case result of 
        Right _ -> createUpdateFromDB t
        Left e -> return $ Left e
    return result'
