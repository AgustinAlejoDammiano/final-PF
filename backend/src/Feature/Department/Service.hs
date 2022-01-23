module Feature.Department.Service where

import ClassyPrelude
import Control.Monad.Except
import Feature.Department.Types
import Feature.Common.Types

class (Monad m) => Dao m where
    listDepartmentsFromDB ::  DepartmentFilter -> Pagination -> m [Department]
    createDepartmentFromDB ::  CreateDepartment -> m(Either DepartmentError Bool)
    deleteDepartmentFromDB :: Integer -> m (Either DepartmentError Bool)
    deleteDepartmentsFromDB :: m (Either DepartmentError Bool)

listDepartments :: (Dao m) => DepartmentFilter -> Pagination -> m [Department]
listDepartments = listDepartmentsFromDB

getDepartment :: (Dao m) => Text -> m(Either DepartmentError Department)
getDepartment name = runExceptT $ do
    result <- lift $ listDepartments (DepartmentFilter $ Just name) (Pagination 1 0)
    case result of
        [department] -> return department
        _ -> throwError $ DepartmentNameNotFound name

createDepartment :: (Dao m) => CreateDepartment -> m(Either DepartmentError Department)
createDepartment param = runExceptT $ do 
    _ <- ExceptT $ createDepartmentFromDB param
    ExceptT $ getDepartment $ createDepartmentName param
    

deleteDepartment :: (Dao m) => Integer -> m (Either DepartmentError Bool)
deleteDepartment = deleteDepartmentFromDB

deleteDepartments :: (Dao m) => m (Either DepartmentError Bool)
deleteDepartments = deleteDepartmentsFromDB
