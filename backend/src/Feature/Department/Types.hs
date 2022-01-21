module Feature.Department.Types where

import ClassyPrelude
import Database.PostgreSQL.Simple.FromRow
import Feature.Common.Utils

data DepartmentFilter = DepartmentFilter{ departmentFilterName :: Maybe Text} deriving (Eq, Show)

data Department = Department{ departmentId :: Integer, departmentName :: Text} deriving (Eq, Show)
data CreateDepartment = CreateDepartment{ createDepartmentId :: Integer, createDepartmentName :: Text} deriving (Eq, Show)
data DepartmentError = DepartmentNotFound Integer | DepartmentNameNotFound Text | DepartmentAlreadyExist Text | UnknownError

newtype DepartmentWrapper a = DepartmentWrapper { departmentWrapperDepartment :: a } deriving (Eq, Show)
data DepartmentsWrapper a = DepartmentsWrapper { departmentsWrapperDepartments :: [a], departmentsWrapperCount :: Int } deriving (Eq, Show)

$(commonJSONDeriveMany
    [ ''Department
    , ''CreateDepartment
    , ''DepartmentError
    , ''DepartmentWrapper
    , ''DepartmentsWrapper
    ])

instance FromRow Department where
    fromRow = Department <$> field <*> field
