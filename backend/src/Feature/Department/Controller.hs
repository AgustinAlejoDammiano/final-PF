module Feature.Department.Controller(routes, Service(..)) where

import ClassyPrelude hiding (delete)

import Feature.Department.Types
import Feature.Common.Types
import Feature.Common.HTTP
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import qualified Text.Digestive.Form as DF
import Text.Digestive.Form ((.:))

class Monad m => Service m where
    listDepartments :: DepartmentFilter -> Pagination -> m [Department]
    getDepartment :: Text -> m(Either DepartmentError Department)
    createDepartment :: CreateDepartment -> m(Either DepartmentError Department)
    deleteDepartment :: Integer -> m (Either DepartmentError Bool)
    deleteDepartments :: m (Either DepartmentError Bool)

routes :: (Service m, MonadIO m) => ScottyT LText m ()
routes = do

    get "/api/department" $ do
        pagination <- parsePagination
        departmentFilter <- parseDepartmentFilter
        result <- lift $ listDepartments departmentFilter pagination
        json $ DepartmentsWrapper result (length result)

    post "/api/department" $ do
        req <- parseJsonBody ("department" .: createDepartmentForm)
        result <- stopIfError departmentErrorHandler $ createDepartment req
        status status201
        json $ DepartmentWrapper result

    delete "/api/department" $ do
        _ <- stopIfError departmentErrorHandler $ deleteDepartments 
        status status204

    delete "/api/department/:id" $ do
        i <- param "id"
        _ <- stopIfError departmentErrorHandler $ deleteDepartment i
        status status204

departmentErrorHandler :: (ScottyError e, Monad m) => DepartmentError -> ActionT e m ()
departmentErrorHandler err = case err of
    DepartmentNotFound _ -> do
        status status404
        json err
    DepartmentNameNotFound _ -> do
        status status404
        json err
    DepartmentAlreadyExist _ -> do
        status status409
        json err
    UnknownError -> do
        status status500
        json err

mayParam :: (ScottyError e, Monad m) => LText -> ActionT e m (Maybe Text)
mayParam name = (Just <$> param name) `rescue` const (return Nothing)

parseDepartmentFilter :: (ScottyError e, Monad m) => ActionT e m DepartmentFilter
parseDepartmentFilter = DepartmentFilter <$> mayParam "name"

createDepartmentForm :: (Monad m) => DF.Form [Text] m CreateDepartment
createDepartmentForm = CreateDepartment <$> "id" .: DF.stringRead ["Not a number"] (Just $ -1)
                                            <*> "name" .: DF.text Nothing
