module Feature.Dose.Service where

import ClassyPrelude
import Control.Monad.Except
import Feature.Dose.Types
import Feature.Common.Types

class (Monad m) => Dao m where
    listDosesFromDB ::  DoseFilter -> Pagination -> m [Dose]
    createDoseFromDB ::  CreateDose -> m(Either DoseError Integer)
    deleteDoseFromDB :: Integer -> m (Either DoseError Bool)
    deleteDosesFromDB :: m (Either DoseError Bool)

listDoses :: (Dao m) => DoseFilter -> Pagination -> m [Dose]
listDoses = listDosesFromDB

getDose :: (Dao m) => Integer -> m(Either DoseError Dose)
getDose i = runExceptT $ do
    result <- lift $ listDoses (DoseFilter (Just i) Nothing) (Pagination 1 0)
    case result of
        [dose] -> return dose
        _ -> throwError $ DoseNameNotFound $ pack $ show i

createDose :: (Dao m) => CreateDose -> m(Either DoseError Dose)
createDose param = do 
    result <- createDoseFromDB param
    dose <- case result of
        Left e -> return $ Left e
        Right i -> getDose i
    return dose

deleteDose :: (Dao m) => Integer -> m (Either DoseError Bool)
deleteDose = deleteDoseFromDB

deleteDoses :: (Dao m) => m (Either DoseError Bool)
deleteDoses = deleteDosesFromDB
