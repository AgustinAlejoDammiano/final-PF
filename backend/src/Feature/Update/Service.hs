module Feature.Update.Service where

import ClassyPrelude
import Feature.Update.Types
import Feature.Common.Types
import Feature.Jurisdiction.Types hiding (UnknownError)
import Feature.Department.Types hiding (UnknownError)
import Data.Time.LocalTime

class Monad m => JurisdictionService m where
    createJurisdiction :: CreateJurisdiction -> m(Either JurisdictionError Jurisdiction)
    deleteJurisdictions :: m (Either JurisdictionError Bool)

class Monad m => DepartmentService m where
    createDepartment :: CreateDepartment -> m(Either DepartmentError Department)
    deleteDepartments :: m (Either DepartmentError Bool)

class Monad m => UpdateDao m where
    listUpdatesFromDB :: Pagination -> m [UpdateDate]
    createUpdateFromDB :: ZonedTime -> m (Either UpdateError Update)

class Monad m => UpdateRepository m where
    loadData :: Text -> (CreateJurisdiction -> m(Either JurisdictionError Jurisdiction)) -> (CreateDepartment -> m(Either DepartmentError Department)) -> m(Either UpdateError Update)

update :: (UpdateRepository m, JurisdictionService m, DepartmentService m, UpdateDao m,  MonadUnliftIO m) => Text -> m(Either UpdateError Update)
update url = do
    t <- liftIO getZonedTime
    deleteResult <- deleteOldData
    result <- case deleteResult of 
        Right _ -> updateData url t
        Left e -> return $ Left e
    return result

listUpdates :: (UpdateDao m) => Pagination -> m [UpdateDate]
listUpdates = listUpdatesFromDB

deleteOldData :: (JurisdictionService m, DepartmentService m) => m (Either UpdateError ())
deleteOldData = do 
    jurisdiction <- deleteJurisdictions
    department <- deleteDepartments
    case lefts [mapEither jurisdiction, mapEither department] of
        [] -> return $ Right ()
        _ -> return $ Left UnknownError

updateData :: (UpdateRepository m, JurisdictionService m, DepartmentService m, UpdateDao m) => Text -> ZonedTime -> m(Either UpdateError Update)
updateData url t = do
    result <- loadData url createJurisdiction createDepartment
    result' <- case result of 
        Right _ -> createUpdateFromDB t
        Left e -> return $ Left e
    return result'

mapEither :: Either a b -> Either Bool Bool
mapEither e = first (\_ -> True) $ second (\_ -> True) e
