module Feature.Department.Dao where

import ClassyPrelude
import Feature.Department.Types
import Feature.Common.Types
import Platform.Database
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Errors
import Data.Either

listDepartmentsFromDB :: Database r m => DepartmentFilter -> Pagination -> m [Department]
listDepartmentsFromDB jf pagination = do
    result <- withConn handler $ \conn -> query conn qry (departmentFilterName jf, paginationLimit pagination, paginationOffset pagination)
    return $ fromRight [] result
    where qry = "select id, name from department where coalesce(?, name) = name limit greatest(0, ?) offset greatest(0, ?)"

createDepartmentFromDB :: Database r m => CreateDepartment -> m (Either DepartmentError Bool)
createDepartmentFromDB param = do
    result <- withConn handler $ connection param
    return $ second (\_ -> True) result
    where 
        connection d
            | (createDepartmentId d) >= 0 = \conn -> execute conn "insert into department (id, name) values (?, ?)" (createDepartmentId d, createDepartmentName d)
            | otherwise = \conn -> execute conn "insert into department (name) values (?)" (Only $ createDepartmentName d)

deleteDepartmentFromDB :: Database r m => Integer -> m (Either DepartmentError Bool)
deleteDepartmentFromDB i = do
    result <- withConn handler $ \conn -> execute conn qry (Only i)
    return $ second (\_ -> True) result
    where qry = "delete from department where id = ?"

deleteDepartmentsFromDB :: Database r m => m (Either DepartmentError Bool)
deleteDepartmentsFromDB = do
    result <- withConn handler $ \conn -> execute conn qry ()
    return $ second (\_ -> True) result
    where qry = "delete from department"

handler :: (SqlError -> ConstraintViolation -> IO (Either DepartmentError a))
handler _ (UniqueViolation x) = return $ Left $ DepartmentAlreadyExist $ decodeUtf8 x
handler _ _ = return $ Left UnknownError
