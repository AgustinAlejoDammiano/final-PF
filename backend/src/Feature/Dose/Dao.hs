module Feature.Dose.Dao where

import ClassyPrelude
import Feature.Dose.Types
import Feature.Common.Types
import Platform.Database
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Errors
import Data.Either

listDosesFromDB :: Database r m => DoseFilter -> Pagination -> m [Dose]
listDosesFromDB df pagination = do
    result <- withConn handler $ \conn -> query conn qry (doseFilterSex df, doseFilterId df, paginationLimit pagination, paginationOffset pagination)
    return $ fromRight [] result
    where qry = "select id, sex, age_group, condition, lot, date, serie, vaccine_id, residence_jurisdiction_id, \
                \residence_department_id, application_jurisdiction_id, application_department_id from dose_application \
                \where coalesce(?, sex) = sex AND coalesce(?, id) = id limit greatest(0, ?) offset greatest(0, ?)"

createDoseFromDB :: Database r m => CreateDose -> m (Either DoseError Integer)
createDoseFromDB param = do
    result <- withConn handler $ \conn -> query conn qry param
    return $ second mapper result
    where 
        qry = "insert into dose_application (sex, age_group, condition, lot, date, serie, vaccine_id, residence_jurisdiction_id, \
                \residence_department_id, application_jurisdiction_id, application_department_id) \
                \values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) returning id"
        mapper r = case r of
            [Only cmtId] -> cmtId
            _ -> -1

deleteDoseFromDB :: Database r m => Integer -> m (Either DoseError Bool)
deleteDoseFromDB i = do
    result <- withConn handler $ \conn -> execute conn qry (Only i)
    return $ second (\_ -> True) result
    where qry = "delete from dose_application where id = ?"

deleteDosesFromDB :: Database r m => m (Either DoseError Bool)
deleteDosesFromDB = do
    result <- withConn handler $ \conn -> execute conn qry ()
    return $ second (\_ -> True) result
    where qry = "delete from dose_application"

handler :: (SqlError -> ConstraintViolation -> IO (Either DoseError a))
handler _ (UniqueViolation x) = return $ Left $ DoseAlreadyExist $ decodeUtf8 x
handler _ _ = return $ Left UnknownError
