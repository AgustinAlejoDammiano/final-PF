module Feature.Vaccine.Service where

import ClassyPrelude
import Control.Monad.Except
import Feature.Vaccine.Types
import Feature.Common.Types

class (Monad m) => Dao m where
    listVaccinesFromDB ::  VaccineFilter -> Pagination -> m [Vaccine]
    createVaccineFromDB ::  CreateVaccine -> m(Either VaccineError Bool)
    deleteVaccineFromDB :: Integer -> m (Either VaccineError Bool)
    deleteVaccinesFromDB :: m (Either VaccineError Bool)

listVaccines :: (Dao m) => VaccineFilter -> Pagination -> m [Vaccine]
listVaccines = listVaccinesFromDB

getVaccine :: (Dao m) => Text -> m(Either VaccineError Vaccine)
getVaccine name = runExceptT $ do
    result <- lift $ listVaccines (VaccineFilter $ Just name) (Pagination 1 0)
    case result of
        [vaccine] -> return vaccine
        _ -> throwError $ VaccineNameNotFound name

createVaccine :: (Dao m) => CreateVaccine -> m(Either VaccineError Vaccine)
createVaccine param = do 
    result <- createVaccineFromDB param
    vaccine <- case result of
        Left e -> return $ Left e
        Right _ -> getVaccine $ createVaccineName param
    return vaccine

deleteVaccine :: (Dao m) => Integer -> m (Either VaccineError Bool)
deleteVaccine = deleteVaccineFromDB

deleteVaccines :: (Dao m) => m (Either VaccineError Bool)
deleteVaccines = deleteVaccinesFromDB
