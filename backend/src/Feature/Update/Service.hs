module Feature.Update.Service where

import ClassyPrelude
import Feature.Update.Types
import Feature.Common.Types
import Feature.Jurisdiction.Types hiding (UnknownError)
import Feature.Department.Types hiding (UnknownError)
import Feature.Vaccine.Types hiding (UnknownError)
import Feature.Dose.Types hiding (UnknownError)
import Data.Time.LocalTime
import Control.Monad.Except

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
update url = runExceptT $ do
    t <- liftIO getZonedTime
    _ <- ExceptT $ deleteOldData
    ExceptT $ updateData url t

listUpdates :: (UpdateDao m) => Pagination -> m [UpdateDate]
listUpdates = listUpdatesFromDB

deleteOldData :: (JurisdictionService m, DepartmentService m, VaccineService m, DoseService m) => m (Either UpdateError ())
deleteOldData = runExceptT $ do 
    _ <- customWithExceptT deleteJurisdictions
    _ <- customWithExceptT deleteDepartments
    _ <- customWithExceptT deleteVaccines
    _ <- customWithExceptT deleteDoses
    return ()
    where customWithExceptT a = withExceptT (\_ -> UnknownError) $ ExceptT a

updateData :: (UpdateRepository m, JurisdictionService m, DepartmentService m, VaccineService m, DoseService m, UpdateDao m) => Text -> ZonedTime -> m(Either UpdateError Update)
updateData url t = runExceptT $ do
    _ <- ExceptT $ loadData url createJurisdiction createDepartment createVaccine createDose
    ExceptT $ createUpdateFromDB t
