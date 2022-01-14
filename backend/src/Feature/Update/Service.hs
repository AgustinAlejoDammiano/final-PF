module Feature.Update.Service where

import ClassyPrelude
import Feature.Update.Types
import Feature.Common.Types
import Feature.Jurisdiction.Types hiding (UnknownError)
import Feature.Department.Types hiding (UnknownError)
import Feature.Vaccine.Types hiding (UnknownError)
import Feature.Dose.Types hiding (UnknownError)
import Data.Time.LocalTime

class Monad m => JurisdictionService m where
    createJurisdiction :: CreateJurisdiction -> m(Either JurisdictionError Jurisdiction)
    deleteJurisdictions :: m (Either JurisdictionError Bool)

class Monad m => DepartmentService m where
    createDepartment :: CreateDepartment -> m(Either DepartmentError Department)
    deleteDepartments :: m (Either DepartmentError Bool)

class Monad m => VaccineService m where
    createVaccine :: CreateVaccine -> m(Either VaccineError Vaccine)
    deleteVaccines :: m (Either VaccineError Bool)

class Monad m => DoseService m where
    createDose :: CreateDose -> m(Either DoseError Dose)
    deleteDoses :: m (Either DoseError Bool)

class Monad m => UpdateDao m where
    listUpdatesFromDB :: Pagination -> m [UpdateDate]
    createUpdateFromDB :: ZonedTime -> m (Either UpdateError Update)

class Monad m => UpdateRepository m where
    loadData :: Text -> (CreateJurisdiction -> m(Either JurisdictionError Jurisdiction)) -> (CreateDepartment -> m(Either DepartmentError Department)) -> (CreateVaccine -> m(Either VaccineError Vaccine)) -> (CreateDose -> m(Either DoseError Dose)) -> m(Either UpdateError Update)

update :: (UpdateRepository m, JurisdictionService m, DepartmentService m, VaccineService m, DoseService m, UpdateDao m,  MonadUnliftIO m) => Text -> m(Either UpdateError Update)
update url = do
    t <- liftIO getZonedTime
    deleteResult <- deleteOldData
    result <- case deleteResult of 
        Right _ -> updateData url t
        Left e -> return $ Left e
    return result

listUpdates :: (UpdateDao m) => Pagination -> m [UpdateDate]
listUpdates = listUpdatesFromDB

deleteOldData :: (JurisdictionService m, DepartmentService m, VaccineService m, DoseService m) => m (Either UpdateError ())
deleteOldData = do 
    jurisdiction <- deleteJurisdictions
    department <- deleteDepartments
    vaccine <- deleteVaccines
    dose <- deleteDoses
    case lefts [mapEither jurisdiction, mapEither department, mapEither vaccine, mapEither dose] of
        [] -> return $ Right ()
        _ -> return $ Left UnknownError

updateData :: (UpdateRepository m, JurisdictionService m, DepartmentService m, VaccineService m, DoseService m, UpdateDao m) => Text -> ZonedTime -> m(Either UpdateError Update)
updateData url t = do
    result <- loadData url createJurisdiction createDepartment createVaccine createDose
    result' <- case result of 
        Right _ -> createUpdateFromDB t
        Left e -> return $ Left e
    return result'

mapEither :: Either a b -> Either Bool Bool
mapEither e = first (\_ -> True) $ second (\_ -> True) e
