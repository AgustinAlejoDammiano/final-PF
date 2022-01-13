module Feature.Vaccine.Dao where

import ClassyPrelude
import Feature.Vaccine.Types
import Feature.Common.Types
import Platform.Database
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Errors
import Data.Either

listVaccinesFromDB :: Database r m => VaccineFilter -> Pagination -> m [Vaccine]
listVaccinesFromDB jf pagination = do
    result <- withConn handler $ \conn -> query conn qry (vaccineFilterName jf, paginationLimit pagination, paginationOffset pagination)
    return $ fromRight [] result
    where qry = "select id, name from vaccine where coalesce(?, name) = name limit greatest(0, ?) offset greatest(0, ?)"

createVaccineFromDB :: Database r m => CreateVaccine -> m (Either VaccineError Bool)
createVaccineFromDB param = do
    result <- withConn handler $ \conn -> execute conn qry (Only $ createVaccineName param)
    return $ second (\_ -> True) result
    where qry = "insert into vaccine (name) values (?)"

deleteVaccineFromDB :: Database r m => Integer -> m (Either VaccineError Bool)
deleteVaccineFromDB i = do
    result <- withConn handler $ \conn -> execute conn qry (Only i)
    return $ second (\_ -> True) result
    where qry = "delete from vaccine where id = ?"

deleteVaccinesFromDB :: Database r m => m (Either VaccineError Bool)
deleteVaccinesFromDB = do
    result <- withConn handler $ \conn -> execute conn qry ()
    return $ second (\_ -> True) result
    where qry = "delete from vaccine"

handler :: (SqlError -> ConstraintViolation -> IO (Either VaccineError a))
handler _ (UniqueViolation x) = return $ Left $ VaccineAlreadyExist $ decodeUtf8 x
handler _ _ = return $ Left UnknownError
